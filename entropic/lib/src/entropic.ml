open Base

module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

type json =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of json list
  | `O of (string * json) list
  ]
[@@deriving schema]

module Make (Entropy : Entropy.S) = struct
  module Monad = struct
    type 'a t = Entropy.t -> Entropy.t * 'a

    let return v entropy = (entropy, v)

    let bind v ~f entropy =
      let entropy, v = v entropy in
      f v entropy

    let map =
      `Custom
        (fun v ~f entropy ->
          let entropy, v = v entropy in
          (entropy, f v))
  end

  include Base.Monad.Make (Monad)

  let ( let+ ) = ( >>| )

  let ( and+! ) l r =
    Let_syntax.Let_syntax.both l r >>| fun (l, r) ->
    Result.Let_syntax.Let_syntax.both l r

  let ( >>=! ) v f =
    v >>= function Result.Ok v -> f v | Result.Error _ as e -> return e

  let ( let*! ) = ( >>=! )

  let ( >>|! ) v f =
    v >>| function Result.Ok v -> Result.Ok (f v) | Result.Error _ as e -> e

  let ( let+! ) = ( >>|! )

  let generate ?(size = 1000) entropy schema =
    let bytes n entropy =
      match Entropy.get entropy n with
      | entropy, Some data -> (entropy, Result.return data)
      | _, None -> (entropy, Result.fail "not enough entropy")
    in
    let byte =
      let+! byte = bytes 1 in
      Char.to_int @@ Bytes.get byte 0
    and int16 =
      let+! bytes = bytes 2 in
      Caml.Bytes.get_int16_ne bytes 0
    and int32 =
      let+! bytes = bytes 4 in
      Caml.Bytes.get_int32_ne bytes 0
    and int64 =
      let+! bytes = bytes 8 in
      Caml.Bytes.get_int64_ne bytes 0
    in
    let bool =
      let+! byte = byte in
      byte land 1 = 1
    and float =
      let+! int = int64 in
      Int64.float_of_bits int
    and int =
      match Sys.word_size_in_bits with
      | 32 -> int32 >>|! Int32.to_int_trunc
      | 64 -> int64 >>|! Int64.to_int_trunc
      | _ -> failwith "invalid word size"
    and string =
      let*! size =
        (* FIXME: Use a logarithmic distribution ? *) byte >>|! fun s -> s % 16
      in
      let+! data = bytes size in
      String.init size ~f:(fun i ->
          let byte = Bytes.get data i |> Char.to_int in
          let char =
            byte land 0x7F |> Char.of_int |> Option.value_exn ~here:[%here]
          in
          if Char.is_print char then char else ' ')
    and time =
      let*! int = int64 in
      let seconds = Int64.float_of_bits int in
      match Ptime.of_float_s seconds with
      | Some time -> return (Ok time)
      | None -> return (Result.failf "invalid time: %f" seconds)
    in
    let range min max =
      if max = 0 then return (Result.return 0)
      else
        let max = max - min in
        let+! n =
          if max <= 0xFF then byte
          else if max <= 0xFFFF then int16
          else if max <= 0x7FFFFFFF then int32 >>|! Int32.to_int_trunc
          else int64 >>|! Int64.to_int_trunc
        in
        (* Not a correct distribution; sue me. *)
        (n % max) + min
    in
    let rec of_descriptor :
        type t.
        int ->
        t Schematic.Schema.descriptor ->
        Entropy.t ->
        Entropy.t * (t, string) Result.t =
     fun size -> function
      | Bool -> bool
      | Date ->
        let+! days = range 0 (365 * 60) in
        Ptime.(
          add_span epoch
            (Span.of_d_ps (days, 0L) |> Option.value_exn ~here:[%here])
          |> Option.value_exn ~here:[%here]
          |> to_date_time |> fst)
      | Fixpoint d -> of_descriptor size @@ Lazy.force d
      | Float -> float
      | Ignore -> return (Result.return ())
      | Int -> int
      | Int64 ->
        let+! bytes = bytes 8 in
        Caml.Bytes.get_int64_ne bytes 0
      | Json -> of_descriptor size json_schema.descriptor
      | List d ->
        let*! len = range 0 size in
        List.init len ~f:(fun _ -> of_descriptor (size / len) d.descriptor)
        |> all >>| Result.all
      | Map { decode; descriptor; _ } ->
        of_descriptor size descriptor >>=! fun v -> return (decode v)
      | Mapping d ->
        let*! len = range 0 size in
        List.init len ~f:(fun _ ->
            let+! key = string
            and+! data = of_descriptor (size / len) d.descriptor in
            (key, data))
        |> all >>| Result.all
      | Null -> return (Result.return ())
      | Nullable d ->
        let*! some = bool in
        if some then of_descriptor (size - 1) d.descriptor >>|! Option.return
        else return (Result.return None)
      | Object { decode; fields; _ } ->
        let size =
          let rec fields_length :
              type fields. fields Schematic.Schema.fields -> int = function
            | FieldEnd -> 0
            | Field { rest; _ } -> 1 + fields_length rest
          in
          size / fields_length fields
        in
        let+! fields =
          let rec decode_fields :
              type fields.
              fields Hlist.t Schematic.Schema.fields ->
              (fields Hlist.t, string) Result.t Monad.t = function
            | FieldEnd -> return (Result.return Hlist.[])
            | Field { field; rest } ->
              let+! field = decode_field field
              and+! rest = decode_fields rest in
              Hlist.(field :: rest)
          and decode_field :
              type t req.
              (t, req) Schematic.Schema.field -> (req, string) Result.t Monad.t
              = function
            | { requirement = Default _; schema; _ } ->
              of_descriptor size
                (Schematic.Schema.field_schema schema).descriptor
            | { requirement = Optional; schema; _ } ->
              let*! some = bool in
              if some then
                of_descriptor size
                  (Schematic.Schema.field_schema schema).descriptor
                >>|! Option.return
              else return (Result.return None)
            | { requirement = Required; schema; _ } ->
              of_descriptor size
                (Schematic.Schema.field_schema schema).descriptor
            | { requirement = Computed; _ } -> return (Result.Ok None)
            | { requirement = Computed_optional; _ } -> return (Result.Ok None)
          in
          decode_fields fields
        in
        decode fields
      | String -> string
      | String_const _ -> return (Result.return ())
      | Time -> time
      | Time_local ->
        let+! time = time
        and+! offset =
          (* FIXME: some timezone have half hours offset *) range (-12) 12
        in
        (time, offset * 3600)
      | Tuple { decode; items; _ } ->
        let size =
          let rec items_length : type items. items Schematic.Schema.tuple -> int
              = function
            | ItemEnd -> 0
            | Item { rest; _ } -> 1 + items_length rest
          in
          size / items_length items
        in
        let+! items =
          let rec decode_items :
              type items.
              items Hlist.t Schematic.Schema.tuple ->
              (items Hlist.t, string) Result.t Monad.t = function
            | ItemEnd -> return (Result.return Hlist.[])
            | Item { schema; rest } ->
              let+! item = of_descriptor size schema.descriptor
              and+! rest = decode_items rest in
              Hlist.(item :: rest)
          in
          decode_items items
        in
        decode items
      | Union { cases; _ } ->
        let*! pick = range 0 List.(length cases) in
        let (Case { schema; decoder; _ }) = List.nth_exn cases pick in
        let+! value = of_descriptor (size - 1) schema.descriptor in
        decoder value
      | Any (module Coding) ->
        (* FIXME *) return (Result.return (Coding.null ()))
    in

    of_descriptor size schema.Schematic.Schema.descriptor entropy |> snd
end

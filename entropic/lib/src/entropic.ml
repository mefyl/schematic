open Base
module Entropy = Entropy

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

module Size : sig
  (** A type representing an integer size.

      It is guaranted to always be positive, any operation that should end-up
      negative round-up to zero *)
  type t

  include Comparable.S with type t := t

  val ( - ) : t -> t -> t
  val ( + ) : t -> t -> t
  val ( / ) : t -> int -> t
  val ( * ) : t -> int -> t
  val ( = ) : t -> t -> bool
  val ( % ) : t -> t -> t
  val of_int : int -> t
  val to_int : t -> int
  val decr : t -> t
  val incr : t -> t
end = struct
  include Int

  let ( - ) a b = max 0 (a - b)
  let decr x = x - 1
  let incr x = x + 1
  let of_int x = max 0 x
  let to_int x = x
end

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
      Stdlib.Bytes.get_int16_ne bytes 0
    and int32 =
      let+! bytes = bytes 4 in
      Stdlib.Bytes.get_int32_ne bytes 0
    and int64 =
      let+! bytes = bytes 8 in
      Stdlib.Bytes.get_int64_ne bytes 0
    in
    let bool =
      let+! byte in
      byte land 1 = 1
    and float =
      let+! int = int64 in
      Int64.float_of_bits int
    and int = int64 >>|! Int64.to_int_trunc
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
    let range ?(fixed = false) min max =
      if max = 0 then return (Result.return 0)
      else
        let max = max - min in
        let+! n =
          if fixed then int64 >>|! Int64.to_int_trunc
          else if max <= 0xFF then byte
          else if max <= 0xFFFF then int16
          else if max <= 0x7FFFFFFF then int32 >>|! Int32.to_int_trunc
          else int64 >>|! Int64.to_int_trunc
        in
        (* Not a correct distribution; sue me. *)
        (n % max) + min
    in
    let rec of_descriptor : type t.
        Size.t ->
        t Schematic.Schema.descriptor ->
        Entropy.t ->
        Entropy.t * (t, string) Result.t =
     fun size -> function
       | Bool -> bool
       | Coding { f } -> of_descriptor size (f None)
       | Date ->
         let+! days = range 0 (365 * 60) in
         Ptime.(
           add_span epoch
             (Span.of_d_ps (days, 0L) |> Option.value_exn ~here:[%here])
           |> Option.value_exn ~here:[%here]
           |> to_date_time
           |> fst)
       | Fixpoint d -> of_descriptor size @@ Lazy.force d
       | Float -> float
       | Ignore -> return (Result.return ())
       | Int -> int
       | Int64 -> int64
       | Json -> of_descriptor size json_schema.descriptor
       | List d ->
         let*! len = range ~fixed:true 0 (Size.to_int size) in
         List.init len ~f:(fun _ ->
             of_descriptor Size.(size / len) d.descriptor)
         |> all
         >>| Result.all
       | Map { decode; descriptor; _ } ->
         of_descriptor size descriptor >>=! fun v -> return (decode v)
       | Mapping { construct; deconstruct; item; length } ->
         of_descriptor size
           (Schematic.Schema.Unversioned.Sequence
              {
                construct;
                deconstruct;
                item = Schematic.Schemas.(Pair.schema string_schema item);
                length;
              })
       | Null -> return (Result.return ())
       | Nullable d ->
         let*! some = bool in
         if some then
           of_descriptor (Size.decr size) d.descriptor >>|! Option.return
         else return (Result.return None)
       | Object { decode; fields; _ } ->
         let size =
           let rec fields_length : type fields.
               fields Schematic.Schema.fields -> int = function
             | FieldEnd -> 0
             | Field { rest; _ } -> 1 + fields_length rest
           in
           Size.(size / fields_length fields)
         in
         let+! fields =
           let rec decode_fields : type fields.
               fields Hlist.t Schematic.Schema.fields ->
               (fields Hlist.t, string) Result.t Monad.t = function
             | FieldEnd -> return (Result.return Hlist.[])
             | Field { field; rest } ->
               let+! field = decode_field field
               and+! rest = decode_fields rest in
               Hlist.(field :: rest)
           and decode_field : type t req.
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
       | Sequence { construct; item; _ } ->
         of_descriptor size (Schematic.Schema.Unversioned.List item)
         >>|! Base.Sequence.of_list
         >>=! Fn.compose Monad.return construct
       | String -> string
       | String_const _ -> return (Result.return ())
       | Time -> time
       | Time_local ->
         let+! time
         and+! offset =
           (* FIXME: some timezone have half hours offset *) range (-12) 12
         in
         (time, offset * 3600)
       | Tuple { decode; items; _ } ->
         let size =
           let rec items_length : type items.
               items Schematic.Schema.tuple -> int = function
             | ItemEnd -> 0
             | Item { rest; _ } -> 1 + items_length rest
           in
           Size.(size / items_length items)
         in
         let+! items =
           let rec decode_items : type items.
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
         let+! value = of_descriptor (Size.decr size) schema.descriptor in
         decoder value
       | Any (module Coding) ->
         (* FIXME *) return (Result.return (Coding.null ()))
    in
    of_descriptor (Size.of_int size) schema.Schematic.Schema.descriptor entropy
    |> snd

  let synthesize schema value =
    let byte buffer char = Buffer.add_char buffer char
    and int16 buffer v =
      let bytes = Bytes.create 2 in
      let () = Stdlib.Bytes.set_int16_ne bytes 0 v in
      Buffer.add_bytes buffer bytes
    and int64 buffer v =
      let bytes = Bytes.create 8 in
      let () = Stdlib.Bytes.set_int64_ne bytes 0 v in
      Buffer.add_bytes buffer bytes
    in
    let bool buffer v = byte buffer (if v then '\x01' else '\x00')
    and float buffer v = int64 buffer @@ Int64.bits_of_float v
    and int buffer v = int64 buffer @@ Int64.of_int v in
    let string buffer v =
      let () = byte buffer @@ Char.of_int_exn @@ String.length v in
      Buffer.add_string buffer v
    and time buffer v = float buffer @@ Ptime.to_float_s v in
    let rec of_descriptor : type t.
        Buffer.t -> t Schematic.Schema.descriptor -> t -> unit =
     fun buffer descriptor value ->
      match descriptor with
      | Bool -> bool buffer value
      | Coding { f } -> of_descriptor buffer (f None) value
      | Date ->
        let open Ptime in
        let days =
          let date_time =
            of_date_time (value, ((0, 0, 0), 0))
            |> Option.value_exn ~here:[%here]
          in
          diff date_time epoch |> Span.to_d_ps |> fst
        in
        int16 buffer days
      | Fixpoint _d -> of_descriptor buffer descriptor value
      | Float -> float buffer value
      | Ignore -> ()
      | Int -> int buffer value
      | Int64 -> int64 buffer value
      | Json -> of_descriptor buffer json_schema.descriptor value
      | List d ->
        let () = int buffer @@ List.length value in
        List.iter value ~f:(fun value ->
            of_descriptor buffer d.descriptor value)
      | Map { encode; descriptor; _ } ->
        of_descriptor buffer descriptor @@ encode value
      | Mapping { construct; deconstruct; item; length } ->
        of_descriptor buffer
          (Schematic.Schema.Unversioned.Sequence
             {
               construct;
               deconstruct;
               item = Schematic.Schemas.(Pair.schema string_schema item);
               length;
             })
          value
      | Null -> ()
      | Nullable d -> (
        match value with
        | None -> bool buffer false
        | Some value ->
          let () = bool buffer true in
          of_descriptor buffer d.descriptor value)
      | Object { encode; fields; _ } ->
        let rec write_fields : type fields.
            fields Hlist.t Schematic.Schema.fields -> fields Hlist.t -> unit =
         fun descriptor fields ->
          match (descriptor, fields) with
          | FieldEnd, [] -> ()
          | Field { field; rest }, head :: tail ->
            let () = write_field field head in
            write_fields rest tail
        and write_field : type t req.
            (t, req) Schematic.Schema.field -> req -> unit =
         fun descriptor field ->
          match descriptor with
          | { requirement = Default _; schema; _ } ->
            of_descriptor buffer
              (Schematic.Schema.field_schema schema).descriptor field
          | { requirement = Optional; schema; _ } -> (
            match field with
            | None -> bool buffer false
            | Some field ->
              let () = bool buffer true in
              of_descriptor buffer
                (Schematic.Schema.field_schema schema).descriptor field)
          | { requirement = Required; schema; _ } ->
            of_descriptor buffer
              (Schematic.Schema.field_schema schema).descriptor field
          | { requirement = Computed; _ } -> ()
          | { requirement = Computed_optional; _ } -> ()
        in
        write_fields fields @@ encode value
      | Sequence { deconstruct; item; _ } ->
        value
        |> deconstruct
        |> Base.Sequence.to_list
        |> of_descriptor buffer (Schematic.Schema.Unversioned.List item)
      | String -> string buffer value
      | String_const _ -> ()
      | Time -> time buffer value
      | Time_local ->
        let () = time buffer @@ fst value in
        byte buffer @@ Char.of_int_exn (snd value / 3600)
      | Tuple { encode; items; _ } ->
        let rec write_items : type items.
            items Hlist.t Schematic.Schema.tuple -> items Hlist.t -> unit =
         fun items value ->
          match (items, value) with
          | ItemEnd, [] -> ()
          | Item { schema; rest }, head :: tail ->
            let () = of_descriptor buffer schema.descriptor head in
            write_items rest tail
        in
        write_items items @@ encode value
      | Union { cases; _ } ->
        let rec write_case n = function
          | [] -> failwith "no case matched"
          | Schematic.Schema.Case { schema; encoder; _ } :: tail -> (
            match encoder value with
            | Some value ->
              let () = byte buffer @@ Char.of_int_exn n in
              of_descriptor buffer schema.descriptor value
            | None -> write_case (n + 1) tail)
        in
        write_case 0 cases
      | Any (module Coding) -> (* FIXME *) ()
    in
    let buffer = Buffer.create 1024 in
    let () = of_descriptor buffer schema.Schematic.Schema.descriptor value in
    Buffer.contents buffer
end

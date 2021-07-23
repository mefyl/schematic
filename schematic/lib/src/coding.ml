module E = Error
open Base

(* FIXME: use Acid.Sequence *)
module List_monad (Monad : Base.Monad.S2) = struct
  include List
  open Monad

  let rec map f = function
    | [] -> Monad.return []
    | hd :: tl ->
      f hd >>= fun hd ->
      map f tl >>= fun tl -> Monad.return (hd :: tl)
end

type 'a customizable =
  [ `Generic
  | `Custom of 'a
  ]

type 't key_get = ('t -> string -> ('t option, string) Result.t) customizable

type 't key_set = ('t -> string -> 't -> ('t, string) Result.t) customizable

module type S = sig
  type t

  val is_null : t -> bool

  val key_get : t key_get

  val key_set : t key_set

  val null : unit -> t

  val of_bool : bool -> t

  val of_date : (Ptime.date -> t) customizable

  val of_fields : (string * t) list -> t

  val of_float : float -> t

  val of_int : int -> t

  val of_int64 : int64 -> t

  val of_json : Ezjsonm.value -> t

  val of_list : t list -> t

  val of_string : string -> t

  val of_time : Ptime.t -> t

  val of_time_local : Ptime.t * int -> t

  val to_bool : t -> bool option

  val to_date : (t -> Ptime.date option) customizable

  val to_fields : t -> (string * t) list option

  val to_float : t -> float option

  val to_int : t -> int option

  val to_int64 : t -> int64 option

  val to_json : t -> Ezjsonm.value

  val to_list : t -> t list option

  val to_string : t -> string option

  val to_time : t -> (Ptime.t, string) Result.t

  val to_time_local : t -> (Ptime.t * int, string) Result.t

  val type_name : t -> string
end

let fail id ids fmt =
  let f reason = E.{ path = ids; schema = id; reason } in
  Fmt.kstr f fmt

let type_error expected effective =
  Fmt.str "expected %s, got %s" expected effective

module Defaults = struct
  let key_get = `Generic

  let key_set = `Generic

  let of_date = `Generic

  let to_date = `Generic
end

module Make (Coding : S) = struct
  type date_tuple = int * int * int [@@deriving schema]

  let ( let* ) = Result.( >>= )

  let type_error id ids expected v = function
    | Some v -> Result.return v
    | None ->
      Result.fail @@ fail id ids "%s"
      @@ type_error expected (Coding.type_name v)

  let key_get schema path o f =
    match Coding.key_get with
    | `Custom impl ->
      impl o f |> Result.map_error ~f:(fun reason -> E.{ path; schema; reason })
    | `Generic ->
      let* fields = Coding.to_fields o |> type_error schema path "object" o in
      List.Assoc.find ~equal:String.equal fields f |> Result.return

  let key_set schema path o f v =
    match Coding.key_set with
    | `Custom impl ->
      impl o f v
      |> Result.map_error ~f:(fun reason -> E.{ path; schema; reason })
    | `Generic ->
      let* fields = Coding.to_fields o |> type_error schema path "object" v in
      Result.return (Coding.of_fields @@ (f, v) :: fields)

  let requirement_return
      (type a req) (req : (a, req) Schema.requirement) (v : a) : req =
    match req with
    | Required -> v
    | Default _ -> v
    | Optional -> Some v

  let decode schema =
    let module List = List_monad (Result) in
    let rec decode_field :
        type t r.
        bool ->
        string option ->
        string list ->
        string ->
        t Schema.t ->
        (t, r) Schema.requirement ->
        Coding.t option ->
        (r, 'error) Result.t =
     fun omit id ids field schema ->
      let ids_next = field :: ids in
      function
      | Required -> (
        function
        | Some v -> decode schema.id ids_next schema.descriptor v
        | None -> Result.fail (fail id ids "missing required field %S" field))
      | Default default -> (
        function
        | Some v ->
          if omit then
            Result.return default
          else
            decode schema.id ids_next schema.descriptor v
        | None -> Result.return default)
      | Optional -> (
        function
        | Some v when Coding.is_null v -> (
          match decode schema.id ids_next schema.descriptor v with
          | Result.Ok v -> Result.return (Some v)
          | Result.Error _ -> Result.return None)
        | Some v ->
          Result.map ~f:Option.return
          @@ decode schema.id ids_next schema.descriptor v
        | None -> Result.return None)
    and decode :
        type t.
        string option ->
        string list ->
        t Schema.descriptor ->
        Coding.t ->
        (t, E.decoding) Result.t =
     fun id ids schema v ->
      let type_error expected res = type_error id ids expected v res
      and error reason =
        let f reason = E.{ path = ids; schema = id; reason } in
        Fmt.kstr f reason
      and ( >>= ) = Result.( >>= ) in
      match schema with
      | Any -> Result.return (Coding.to_json v)
      | Bool -> Coding.to_bool v |> type_error "boolean"
      | Date -> (
        match Coding.to_date with
        | `Generic -> decode None ids date_tuple_schema.descriptor v
        | `Custom f -> f v |> type_error "date")
      | Float -> Coding.to_float v |> type_error "float"
      | Ignore -> Result.return ()
      | Int -> Coding.to_int v |> type_error "integer"
      | Int64 -> Coding.to_int64 v |> type_error "integer"
      | List items_schema ->
        let* v = Coding.to_list v |> type_error "array" in
        List.map (decode None ids items_schema.descriptor) v
      | Map (cons, _, schema) ->
        decode id ids schema v >>= fun value ->
        cons value |> Result.map_error ~f:(fun reason -> error "%s" reason)
      | Mapping fields_schema ->
        let* fields = Coding.to_fields v |> type_error "mapping" in
        let f (name, value) =
          decode fields_schema.id ids fields_schema.descriptor value
          >>= fun value -> Result.return (name, value)
        in
        List.map f fields
      | Null -> Option.some_if (Coding.is_null v) () |> type_error "null"
      | Nullable s ->
        if Coding.is_null v then
          Result.return None
        else
          decode s.id ids s.descriptor v |> Result.map ~f:Option.return
      | Object fields_schema ->
        let* fields = Coding.to_fields v |> type_error "object" in
        let rec decode_object :
            type t. t Schema.field -> (t, E.decoding) Result.t = function
          | FieldEnd -> Result.return ()
          | Field { field; omit; requirement; rest; schema; _ } -> (
            match Schema.extract_field_schema schema with
            | _, { descriptor = Ignore; _ } ->
              let* rest = decode_object rest in
              Result.Ok (requirement_return requirement (), rest)
            | inline, schema ->
              let field =
                let missing =
                  if inline then
                    let expected = Schema.descriptor_fields schema.descriptor in
                    Option.is_some
                    @@ List.find
                         ~f:(fun f ->
                           not (List.Assoc.mem ~equal:String.equal fields f))
                         expected
                  else
                    false
                in
                if missing then
                  decode_field omit id ids field schema requirement None
                else
                  let value =
                    if inline then
                      Some v
                    else
                      List.Assoc.find ~equal:String.equal fields field
                  in
                  decode_field omit id ids field schema requirement value
              in
              Result.Let_syntax.Let_syntax.both field (decode_object rest))
        in
        decode_object fields_schema
      | String -> Coding.to_string v |> type_error "string"
      | String_const expected -> (
        match Coding.to_string v with
        | Some s when String.(s = expected) -> Result.return ()
        | Some s -> Result.fail (error "expected %S, got %S" expected s)
        | None -> type_error "string" None)
      | Time -> Coding.to_time v |> Result.map_error ~f:(error "%s")
      | Time_local -> Coding.to_time_local v |> Result.map_error ~f:(error "%s")
      | Tuple items ->
        let* v = Coding.to_list v |> type_error "list" in
        let rec decode_tuple :
            type t. t Schema.tuple -> Coding.t list -> (t, E.decoding) Result.t
            =
         fun t v ->
          match (t, v) with
          | Item { schema; rest }, hd :: tl ->
            decode schema.id ids schema.descriptor hd >>= fun hd ->
            decode_tuple rest tl >>= fun tl -> Result.return (hd, tl)
          | ItemEnd, [] -> Result.return ()
          | Item _, [] -> Result.fail (error "not enough tuple elements")
          | ItemEnd, _ -> Result.fail (error "too many tuple elements")
        in
        decode_tuple items v
      | Union { cases; key } -> (
        match key with
        | None -> (
          let conv (Schema.Case { decoder; schema; _ }) =
            let id = Option.first_some schema.id id in
            match decode id ids schema.descriptor v with
            | Result.Ok v -> Some (decoder v)
            | Result.Error _ -> None
          in
          match List.find_map ~f:conv cases with
          | Some res -> Result.return res
          | None -> Result.fail (error "no union case was matched"))
        | Some key -> (
          let* key_value =
            let* v =
              match key_get id ids v key with
              | Result.Ok (Some v) -> Result.return v
              | Result.Ok None ->
                Result.Error (error "missing key field %S" key)
              | Result.Error e -> Result.Error e
            in
            Coding.to_string v |> type_error "string"
          in
          let conv (Schema.Case { decoder; name; schema; _ }) =
            let id = Option.first_some schema.id id in
            if String.(name = key_value) then
              Option.return
              @@ let* v = decode id ids schema.descriptor v in
                 Result.return (decoder v)
            else
              None
          in
          match List.find_map ~f:conv cases with
          | Some res -> res
          | None ->
            Result.fail (error "invalid key field %S = %S" key key_value)))
    in
    decode schema.Schema.id [] schema.descriptor

  let encode schema =
    let encode_field : type t r. (t, r) Schema.requirement -> r -> t option =
      function
      | Required -> Option.some
      | Default default -> (
        function
        | v when Poly.(v = default (* FIXME: Poly rather bold *)) -> None
        | v -> Some v)
      | Optional -> Fn.id
    in
    let rec encode : type t. t Schema.descriptor -> t -> Coding.t =
     fun schema v ->
      match schema with
      | Any -> Coding.of_json v
      | Bool -> Coding.of_bool v
      | Date -> (
        match Coding.of_date with
        | `Generic -> encode date_tuple_schema.descriptor v
        | `Custom f -> f v)
      | Float -> Coding.of_float v
      | Ignore -> Coding.null ()
      | Int -> Coding.of_int v
      | Int64 -> Coding.of_int64 v
      | List items_schema ->
        Coding.of_list (List.map ~f:(encode items_schema.descriptor) v)
      | Map (_, decons, schema) -> encode schema @@ decons v
      | Null -> Coding.null ()
      | Nullable s -> (
        match v with
        | None -> Coding.null ()
        | Some v -> encode s.descriptor v)
      | Mapping fields_schema ->
        Coding.of_fields
          (List.map
             ~f:(fun (name, v) -> (name, encode fields_schema.descriptor v))
             v)
      | Object fields ->
        let rec encode_object :
            type t. t -> t Schema.field -> (string * Coding.t) list =
         fun v -> function
          | FieldEnd -> []
          | Field { schema = Inline { descriptor = Ignore; _ }; rest; _ } ->
            encode_object (snd v) rest
          | Field { schema = Outline { descriptor = Ignore; _ }; rest; _ } ->
            encode_object (snd v) rest
          | Field { field; requirement; schema; rest; _ } -> (
            let inline, schema = Schema.extract_field_schema schema
            and hd, tl = v in
            let tl = encode_object tl rest in
            match encode_field requirement hd with
            | Some hd ->
              if inline then
                Schema.inline_v schema.descriptor hd
                  { f = (fun fields v -> encode_object v fields) }
                @ tl
              else
                let encoded = encode schema.descriptor hd in
                (field, encoded) :: tl
            | None -> tl)
        in
        Coding.of_fields (encode_object v fields)
      | Tuple items ->
        let rec encode_tuple : type t. t -> t Schema.tuple -> Coding.t list =
         fun v -> function
          | ItemEnd -> []
          | Item { schema; rest } ->
            let hd, tl = v in
            let hd = encode schema.descriptor hd in
            hd :: encode_tuple tl rest
        in
        Coding.of_list (encode_tuple v items)
      | String -> Coding.of_string v
      | String_const v -> Coding.of_string v
      | Time -> Coding.of_time v
      | Time_local -> Coding.of_time_local v
      | Union { cases; key } -> (
        let conv (Schema.Case { encoder; name; schema; _ }) =
          match encoder v with
          | Some v -> (
            let v = encode schema.descriptor v in
            match key with
            | Some key -> (
              match
                key_set None [ (* FIXME: context *) ] v key
                  (Coding.of_string name)
              with
              | Result.Ok v -> Some v
              | Result.Error _ -> Some v)
            | None -> Some v)
          | None -> None
        in
        match List.find_map ~f:conv cases with
        | Some res -> res
        | None -> failwith "unmatched variant schema case")
    in
    encode schema.Schema.descriptor
end

open Base

(* FIXME: use Acid.Sequence *)
module List_monad (Monad : Monad.S2) = struct
  include List
  open Monad

  let rec map f = function
    | [] -> Monad.return []
    | hd :: tl ->
      f hd >>= fun hd ->
      map f tl >>= fun tl -> Monad.return (hd :: tl)
end

type error = [ `Decode of string ]

module type S = sig
  type t

  val is_null : t -> bool

  val to_bool : t -> bool option

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

let type_error expected effective =
  Result.fail Fmt.(str "expected %s, got %s" expected effective)

module Make (Coding : S) = struct
  type date_tuple = int * int * int [@@deriving schema]

  let ( let* ) = Result.( >>= )

  let decode schema =
    let module List = List_monad (Result) in
    let rec decode_field :
        type t r.
        string option ->
        string ->
        t Schema.schema ->
        (t, r) Schema.field_requirement ->
        Coding.t option ->
        (r, 'error) Result.t =
     fun id field schema -> function
      | Required -> (
        function
        | Some v -> decode schema.id schema.descriptor v
        | None -> (
          match id with
          | None ->
            Result.fail
              (`Decode (Printf.sprintf "missing required field %S" field))
          | Some id ->
            Result.fail
              (`Decode
                (Printf.sprintf "missing required field %S in schema %S" field
                   id))))
      | Default default -> (
        function
        | Some v -> decode schema.id schema.descriptor v
        | None -> Result.return default)
      | Optional -> (
        function
        | Some v when Coding.is_null v -> (
          match decode schema.id schema.descriptor v with
          | Result.Ok v -> Result.return (Some v)
          | Result.Error _ -> Result.return None)
        | Some v ->
          Result.map ~f:Option.return @@ decode schema.id schema.descriptor v
        | None -> Result.return None)
    and decode :
        type t.
        string option -> t Schema.descriptor -> Coding.t -> (t, 'error) Result.t
        =
     fun id schema v ->
      let type_error expected = function
        | Some v -> Result.return v
        | None ->
          type_error expected (Coding.type_name v)
          |> Result.map_error ~f:(fun e -> `Decode e)
      in
      let open Result in
      match schema with
      | Any -> Result.return (Coding.to_json v)
      | Bool -> Coding.to_bool v |> type_error "boolean"
      | Date -> decode None date_tuple_schema.descriptor v
      | Float -> Coding.to_float v |> type_error "float"
      | Ignore -> Result.return ()
      | Int -> Coding.to_int v |> type_error "integer"
      | Int64 -> Coding.to_int64 v |> type_error "integer"
      | List items_schema ->
        let* v = Coding.to_list v |> type_error "array" in
        List.map (decode None items_schema.descriptor) v
      | Map (cons, _, schema) ->
        decode id schema v >>= fun value -> Result.return @@ cons value
      | Mapping fields_schema ->
        let* fields = Coding.to_fields v |> type_error "mapping" in
        let f (name, value) =
          decode fields_schema.id fields_schema.descriptor value
          >>= fun value -> Result.return (name, value)
        in
        List.map f fields
      | Null -> Option.some_if (Coding.is_null v) () |> type_error "null"
      | Nullable s ->
        if Coding.is_null v then
          Result.return None
        else
          decode s.id s.descriptor v |> Result.map ~f:Option.return
      | Object fields_schema ->
        let* fields = Coding.to_fields v |> type_error "object" in
        let rec decode_object :
            type t. t Schema.field -> (t, [> `Decode of string ]) Result.t =
          function
          | FieldEnd -> Result.return ()
          | Field { field; requirement; schema; rest; _ } ->
            let inline, schema = Schema.extract_field_schema schema in
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
                decode_field id field schema requirement None
              else
                let value =
                  if inline then
                    Some v
                  else
                    List.Assoc.find ~equal:String.equal fields field
                in
                decode_field id field schema requirement value
            in
            Result.Let_syntax.Let_syntax.both field (decode_object rest)
        in
        decode_object fields_schema
      | String -> Coding.to_string v |> type_error "string"
      | String_const expected -> (
        match Coding.to_string v with
        | Some s when String.(s = expected) -> Result.return ()
        | Some s ->
          Result.fail
            (`Decode (Printf.sprintf "expected %S, got %S" expected s))
        | None -> type_error "string" None)
      | Time -> Coding.to_time v |> Result.map_error ~f:(fun e -> `Decode e)
      | Time_local ->
        Coding.to_time_local v |> Result.map_error ~f:(fun e -> `Decode e)
      | Tuple items ->
        let* v = Coding.to_list v |> type_error "list" in
        let rec decode_tuple :
            type t.
            t Schema.tuple ->
            Coding.t list ->
            (t, [> `Decode of string ]) Result.t =
         fun t v ->
          match (t, v) with
          | Item { schema; rest }, hd :: tl ->
            decode schema.id schema.descriptor hd >>= fun hd ->
            decode_tuple rest tl >>= fun tl -> Result.return (hd, tl)
          | ItemEnd, [] -> Result.return ()
          | Item _, [] -> Result.fail (`Decode "not enough tuple elements")
          | ItemEnd, _ -> Result.fail (`Decode "too many tuple elements")
        in
        decode_tuple items v
      | Union { cases; key } -> (
        let conv (Schema.Case { decoder; name; schema; _ }) =
          let key =
            match key with
            | None -> true
            | Some key -> (
              match Coding.to_fields v with
              | Some fields -> (
                match
                  List.Assoc.find fields ~equal:String.equal key
                  |> Option.bind ~f:Coding.to_string
                with
                | Some n -> String.(n = name)
                | _ -> false)
              | _ -> false)
          in
          if key then
            match decode schema.id schema.descriptor v with
            | Result.Ok v -> Some (decoder v)
            | Result.Error _ -> None
          else
            None
        in
        match List.find_map ~f:conv cases with
        | Some res -> Result.return res
        | None -> Result.fail (`Decode "no union case was matched"))
    in
    decode schema.Schema.id schema.descriptor
end

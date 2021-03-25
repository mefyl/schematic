open Base
open Poly
include Json_intf
open Schema

module Make (Impl : Impl) = struct
  let type_name = function
    | `O _ -> "object"
    | `A _ -> "array"
    | `Bool _ -> "bool"
    | `Float _ -> "float"
    | `String _ -> "string"
    | `Null -> "null"

  module List_monad (Monad : Monad.S2) = struct
    include List
    open Monad

    let rec map f = function
      | [] -> Monad.return []
      | hd :: tl ->
        f hd >>= fun hd ->
        map f tl >>= fun tl -> Monad.return (hd :: tl)
  end

  type date_tuple = int * int * int [@@deriving schema]

  type ('t, 'kind) object_descriptor = ('t, 'kind) descriptor
    constraint 'kind = [> `Object ]

  let decode schema =
    let rec decode_field :
        type t r.
        string option ->
        string ->
        t schema ->
        (t, r) field_requirement ->
        Ezjsonm.value option ->
        (r, 'error) Result.t =
     fun id field schema -> function
      | Required -> (
        function
        | Some v -> decode schema.id schema.descriptor v
        | None -> (
          match id with
          | None ->
            Result.fail
              (`Json_decode (Printf.sprintf "missing required field %S" field))
          | Some id ->
            Result.fail
              (`Json_decode
                (Printf.sprintf "missing required field %S in schema %S" field
                   id))))
      | Default default -> (
        function
        | Some v -> decode schema.id schema.descriptor v
        | None -> Result.return default)
      | Optional -> (
        function
        | Some `Null -> (
          match decode schema.id schema.descriptor `Null with
          | Result.Ok v -> Result.return (Some v)
          | Result.Error _ -> Result.return None)
        | Some v ->
          Result.map ~f:Option.return @@ decode schema.id schema.descriptor v
        | None -> Result.return None)
    and decode :
        type t kind.
        string option ->
        (t, kind) descriptor ->
        Ezjsonm.value ->
        (t, 'error) Result.t =
     fun id schema json ->
      let module List = List_monad (Result) in
      let type_error expected =
        Result.fail
          (`Json_decode
            (Printf.sprintf "expected %s, got %s" expected (type_name json)))
      in
      let open Result in
      match (schema, json) with
      | Any, v -> Result.return v
      | Bool, `Bool b -> Result.return b
      | Bool, _ -> type_error "boolean"
      | Date, json -> decode None date_tuple_schema.descriptor json
      | Float, `Float f -> Result.return f
      | Ignore, _ -> Result.return ()
      | Int, `Float f -> Result.return @@ Float.to_int f
      | Int64, `Float f -> Result.return @@ Float.to_int64 f
      | Float, _
      | Int, _
      | Int64, _ ->
        type_error "float"
      | List items_schema, `A items ->
        List.map (decode None items_schema.descriptor) items
      | List _, _ -> type_error "list"
      | Map (cons, _, schema), _ ->
        decode id schema json >>= fun value -> Result.return @@ cons value
      | Mapping fields_schema, `O fields ->
        let f (name, value) =
          decode fields_schema.id fields_schema.descriptor value
          >>= fun value -> Result.return (name, value)
        in
        List.map f fields
      | Mapping _, _ -> type_error "mapping"
      | Null, `Null -> Result.return ()
      | Null, _ -> type_error "null"
      | Nullable _, `Null -> Result.return None
      | Nullable s, v ->
        decode s.id s.descriptor v |> Result.map ~f:Option.return
      | Object descriptor, (`O fields as o) ->
        let rec decode_object :
            type t.
            (t, [ `Object ]) object_descriptor ->
            (t, [> decode_error ]) Result.t = function
          | Null -> Result.return ()
          | Object { field; requirement; schema; rest; _ } ->
            let inline, schema = extract_field_schema schema in
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
                    Some o
                  else
                    List.Assoc.find ~equal:( = ) fields field
                in
                decode_field id field schema requirement value
            in
            Result.Let_syntax.Let_syntax.both field (decode_object rest)
        in
        decode_object (Object descriptor)
      | Object _, _ -> type_error "object"
      | String, `String s -> Result.return s
      | String, _ -> type_error "string"
      | String_const expected, `String effective when effective = expected ->
        Result.return ()
      | String_const expected, `String effective ->
        Result.fail
          (`Json_decode
            (Printf.sprintf "expected \"%s\", got \"%s\""
               (String.escaped expected) (String.escaped effective)))
      | String_const _, _ -> type_error "string"
      | Time, `String s ->
        let open Result in
        let error (`RFC3339 (_, e)) =
          `Json_decode (Caml.Format.asprintf "%a" Ptime.pp_rfc3339_error e)
        in
        Ptime.of_rfc3339 ~strict:true s
        >>| (fun (t, _tz, _) -> t)
        |> Result.map_error ~f:error
      | Time_local, `String s -> (
        let open Result in
        let error (`RFC3339 (_, e)) =
          `Json_decode (Caml.Format.asprintf "%a" Ptime.pp_rfc3339_error e)
        in
        Ptime.of_rfc3339 ~strict:true s |> Result.map_error ~f:error
        >>= function
        | t, Some tz, _ -> Result.return (t, tz)
        | _, None, _ -> Result.fail (`Json_decode "missing timezone"))
      | Time, _ -> type_error "time"
      | Time_local, _ -> type_error "time"
      | Tuple { schema; rest = Null }, `A [ hd ] ->
        decode schema.id schema.descriptor hd >>= fun hd ->
        Result.return (hd, ())
      | Tuple { schema = _; rest = Null }, `A [] ->
        Result.fail (`Json_decode "not enough tuple elements")
      | Tuple { schema = _; rest = Null }, `A _ ->
        Result.fail (`Json_decode "too many tuple elements")
      | Tuple { schema; rest }, `A (hd :: tl) ->
        decode schema.id schema.descriptor hd >>= fun hd ->
        decode id rest (`A tl) >>= fun tl -> Result.return (hd, tl)
      | Tuple _, _ -> type_error "tuple"
      | Union { cases; key }, _ -> (
        let conv (Case { decoder; name; schema; _ }) =
          let key =
            match key with
            | None -> true
            | Some key -> (
              match json with
              | `O fields -> (
                match List.Assoc.find fields ~equal:String.equal key with
                | Some (`String n) -> String.(n = name)
                | _ -> false)
              | _ -> false)
          in
          if key then
            match decode schema.id schema.descriptor json with
            | Result.Ok v -> Some (decoder v)
            | Result.Error _ -> None
          else
            None
        in
        match List.find_map ~f:conv cases with
        | Some res -> Result.return res
        | None -> Result.fail (`Json_decode "no union case was matched"))
    in
    decode schema.id schema.descriptor

  let encode_field : type t r. (t, r) field_requirement -> r -> t option =
    function
    | Required -> Option.some
    | Default default -> (
      function
      | v when v = default -> None
      | v -> Some v)
    | Optional -> Fn.id

  let encode schema =
    let rec encode : type t kind. (t, kind) descriptor -> t -> Ezjsonm.value =
     fun schema v ->
      match schema with
      | Any -> v
      | Bool -> `Bool v
      | Date -> encode date_tuple_schema.descriptor v
      | Float -> `Float v
      | Ignore -> `Null
      | Int -> `Float (Float.of_int v)
      | Int64 -> `Float (Float.of_int64 v)
      | List items_schema -> `A (List.map ~f:(encode items_schema.descriptor) v)
      | Map (_, decons, schema) -> encode schema @@ decons v
      | Null -> `Null
      | Nullable s -> (
        match v with
        | None -> `Null
        | Some v -> encode s.descriptor v)
      | Mapping fields_schema ->
        `O
          (List.map
             ~f:(fun (name, v) -> (name, encode fields_schema.descriptor v))
             v)
      | Object descriptor ->
        let rec encode_object :
            type t.
            t -> (t, [ `Object ]) descriptor -> (string * Ezjsonm.value) list =
         fun v -> function
          | Null -> []
          | Object { field; requirement; schema; rest; _ } -> (
            let inline, schema = extract_field_schema schema
            and hd, tl = v in
            let tl = encode_object tl rest in
            match encode_field requirement hd with
            | Some hd ->
              if inline then
                match schema.descriptor with
                | Null -> []
                | Object descriptor -> encode_object hd (Object descriptor) @ tl
                | Map (_, f, Object descriptor) ->
                  encode_object (f hd) (Object descriptor) @ tl
                | _ -> failwith "invalid object schema"
              else
                let encoded = encode schema.descriptor hd in
                (field, encoded) :: tl
            | None -> tl)
        in
        `O (encode_object v (Object descriptor))
      | Tuple descriptor ->
        let rec encode_tuple :
            type t. t -> (t, [ `Tuple ]) descriptor -> Ezjsonm.value list =
         fun v -> function
          | Null -> []
          | Tuple { schema; rest } ->
            let hd, tl = v in
            let hd = encode schema.descriptor hd in
            hd :: encode_tuple tl rest
        in
        `A (encode_tuple v (Tuple descriptor))
      | String -> `String v
      | String_const v -> `String v
      | Time -> `String (Ptime.to_rfc3339 v)
      | Time_local ->
        let t, tz_offset_s = v in
        `String (Ptime.to_rfc3339 ~tz_offset_s t)
      | Union { cases; key } -> (
        let conv (Case { encoder; name; schema; _ }) =
          match encoder v with
          | Some v -> (
            match (key, encode schema.descriptor v) with
            | Some key, `O fields -> Some (`O ((key, `String name) :: fields))
            | _, res -> Some res)
          | None -> None
        in
        match List.find_map ~f:conv cases with
        | Some res -> res
        | None -> failwith "unmatched variant schema case")
    in
    encode schema.descriptor

  let from_channel c =
    match Ezjsonm.value_from_channel c with
    | v -> Result.return v
    | exception Ezjsonm.Parse_error (_, msg) -> Result.fail @@ `Parse msg

  let of_string s = Impl.of_string s

  let to_channel chan t = Ezjsonm.value_to_channel ~minify:true chan t

  let to_string t = Impl.to_string t

  let rec pp f json =
    let pp =
      let open Fmt in
      let indent = 2 in
      match json with
      | `Bool b -> const bool b
      | `Float f -> const float f
      | `String s -> const Dump.string s
      | `Null -> const string "null"
      | `A elements ->
        box
        @@ const string "["
           ++ (hvbox ~indent @@ (cut ++ const (list ~sep:comma pp) elements))
           ++ cut ++ const string "]"
      | `O elements ->
        box
        @@ const string "{"
           ++ (hvbox ~indent
              @@ cut
                 ++ const
                      (list ~sep:comma
                         (box ~indent
                         @@ pair ~sep:(const string ":" ++ sp) Dump.string pp))
                      elements)
           ++ cut ++ const string "}"
    in
    pp f ()

  let to_ezjsonm v = v
end

module Impl_ezjsonm = struct
  let of_string s =
    match Ezjsonm.value_from_string s with
    | v -> Result.return v
    | exception Ezjsonm.Parse_error (_, msg) -> Result.fail @@ `Parse msg

  let to_string t = Ezjsonm.value_to_string ~minify:true t
end

include Make (Impl_ezjsonm)

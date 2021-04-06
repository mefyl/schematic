open Base
open Poly
include Json_intf
open Schema

module Json_coding : Coding.S with type t = Ezjsonm.value = struct
  type t = Ezjsonm.value

  let ( >>= ) = Result.( >>= )

  let ( >>| ) = Result.( >>| )

  let is_null = function
    | `Null -> true
    | _ -> false

  let type_name = function
    | `O _ -> "object"
    | `A _ -> "array"
    | `Bool _ -> "bool"
    | `Float _ -> "float"
    | `String _ -> "string"
    | `Null -> "null"

  let to_bool = function
    | `Bool v -> Some v
    | _ -> None

  let to_fields = function
    | `O fields -> Some fields
    | _ -> None

  let to_float = function
    | `Float v -> Some v
    | _ -> None

  let to_int = function
    | `Float v -> Some (Float.to_int v)
    | _ -> None

  let to_int64 = function
    | `Float v -> Some (Float.to_int64 v)
    | _ -> None

  let to_json v = v

  let to_list = function
    | `A l -> Some l
    | _ -> None

  let to_string = function
    | `String v -> Some v
    | _ -> None

  let to_time_ = function
    | `String s ->
      let error (`RFC3339 (_, e)) =
        Caml.Format.asprintf "%a" Ptime.pp_rfc3339_error e
      in
      Ptime.of_rfc3339 ~strict:true s |> Result.map_error ~f:error
    | v -> Coding.type_error "string" (type_name v)

  let to_time v = to_time_ v >>| fun (t, _tz, _) -> t

  let to_time_local v =
    to_time_ v >>= function
    | t, Some tz, _ -> Result.return (t, tz)
    | _, None, _ -> Result.fail "missing timezone"
end

module Make (Impl : Impl) = struct
  include Coding.Make (Json_coding)

  let decode schema json =
    match decode schema json with
    | Result.Error (`Decode r) -> Result.Error (`Json_decode r)
    | Result.Ok _ as v -> v

  let encode_field : type t r. (t, r) field_requirement -> r -> t option =
    function
    | Required -> Option.some
    | Default default -> (
      function
      | v when v = default -> None
      | v -> Some v)
    | Optional -> Fn.id

  let encode schema =
    let rec encode : type t. t descriptor -> t -> Ezjsonm.value =
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
      | Object fields ->
        let rec encode_object :
            type t. t -> t field -> (string * Ezjsonm.value) list =
         fun v -> function
          | FieldEnd -> []
          | Field { field; requirement; schema; rest; _ } -> (
            let inline, schema = extract_field_schema schema
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
        `O (encode_object v fields)
      | Tuple items ->
        let rec encode_tuple : type t. t -> t tuple -> Ezjsonm.value list =
         fun v -> function
          | ItemEnd -> []
          | Item { schema; rest } ->
            let hd, tl = v in
            let hd = encode schema.descriptor hd in
            hd :: encode_tuple tl rest
        in
        `A (encode_tuple v items)
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

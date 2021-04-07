open Base
include Json_intf

module Json_coding : Coding.S with type t = Ezjsonm.value = struct
  type t = Ezjsonm.value

  let set_key = `Generic

  let type_name = function
    | `O _ -> "object"
    | `A _ -> "array"
    | `Bool _ -> "bool"
    | `Float _ -> "float"
    | `String _ -> "string"
    | `Null -> "null"

  let ( >>= ) = Result.( >>= )

  let ( >>| ) = Result.( >>| )

  let is_null = function
    | `Null -> true
    | _ -> false

  let null = `Null

  let of_bool b = `Bool b

  let of_fields f = `O f

  let of_float f = `Float f

  let of_int i = `Float (Float.of_int i)

  let of_int64 i = `Float (Float.of_int64 i)

  let of_json v = v

  let of_list l = `A l

  let of_string s = `String s

  let of_time t = `String (Ptime.to_rfc3339 t)

  let of_time_local (t, tz_offset_s) = `String (Ptime.to_rfc3339 ~tz_offset_s t)

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
    | v -> Result.fail @@ Coding.type_error "string" (type_name v)

  let to_time v = to_time_ v >>| fun (t, _tz, _) -> t

  let to_time_local v =
    to_time_ v >>= function
    | t, Some tz, _ -> Result.return (t, tz)
    | _, None, _ -> Result.fail "missing timezone"
end

module Make (Impl : Impl) = struct
  include Coding.Make (Json_coding)

  let from_channel c =
    match Ezjsonm.value_from_channel c with
    | v -> Result.return v
    | exception Ezjsonm.Parse_error (_, msg) -> Result.fail msg

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
    | exception Ezjsonm.Parse_error (_, msg) -> Result.fail msg

  let to_string t = Ezjsonm.value_to_string ~minify:true t
end

include Make (Impl_ezjsonm)

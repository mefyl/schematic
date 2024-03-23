open Base
include Json_intf

let schema = Schema.make Json
let schema_versioned (_ : Schema.version) = schema

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

type Coding.any += Json of Ezjsonm.value

module Impl : Coding.IMPL with type t = Ezjsonm.value = struct
  let coding = Coding

  type t = Ezjsonm.value

  let name = "JSON"
  let pp = pp
  let compare = Schema.compare schema

  include Coding.Defaults

  let type_name = function
    | `O _ -> "object"
    | `A _ -> "array"
    | `Bool _ -> "bool"
    | `Float _ -> "float"
    | `String _ -> "string"
    | `Null -> "null"

  let ( >>= ) = Result.( >>= )
  let ( >>| ) = Result.( >>| )
  let null () = `Null
  let of_any = function Json v -> Some v | _ -> None
  let of_bool b = `Bool b
  let of_fields f = `O f
  let of_float f = `Float f
  let of_int i = `Float (Float.of_int i)
  let of_int64 i = `Float (Float.of_int64 i)
  let of_json v = v
  let of_list l = `A l
  let of_option = function Some v -> v | None -> `Null
  let of_string s = `String s
  let of_time t = `String (Ptime.to_rfc3339 t)
  let of_time_local (t, tz_offset_s) = `String (Ptime.to_rfc3339 ~tz_offset_s t)
  let to_any = function v -> Json v
  let to_bool = function `Bool v -> Some v | _ -> None
  let to_fields = function `O fields -> Some fields | _ -> None
  let to_float = function `Float v -> Some v | _ -> None
  let to_int = function `Float v -> Some (Float.to_int v) | _ -> None
  let to_int64 = function `Float v -> Some (Float.to_int64 v) | _ -> None
  let to_json v = Some v
  let to_list = function `A l -> Some l | _ -> None
  let to_option = function `Null -> Some None | v -> Some (Some v)
  let to_string = function `String v -> Some v | _ -> None

  let to_time_ = function
    | `String s ->
      let error (`RFC3339 (_, e)) =
        Some (Stdlib.Format.asprintf "%a" Ptime.pp_rfc3339_error e)
      in
      Ptime.of_rfc3339 ~strict:true s |> Result.map_error ~f:error
    | _ -> Result.fail None

  let to_time v = to_time_ v >>| fun (t, _tz, _) -> t

  let to_time_local v =
    to_time_ v >>= function
    | t, Some tz, _ -> Result.return (t, tz)
    | _, None, _ -> Result.fail (Some "missing timezone")
end

module Make (Json_impl : Impl) = struct
  include Coding.Make (Impl)

  let from_channel c =
    match Ezjsonm.value_from_channel c with
    | v -> Result.return v
    | exception Ezjsonm.Parse_error (_, msg) -> Result.fail msg

  let content_type = "application/json"
  let of_string s = Json_impl.of_string s
  let to_channel chan t = Ezjsonm.value_to_channel ~minify:true chan t
  let to_string t = Json_impl.to_string t
  let pp = pp
  let to_ezjsonm v = v

  let rec compare l r =
    match (l, r) with
    | `Null, `Null -> 0
    | `Null, _ -> -1
    | _, `Null -> 1
    | `Bool l, `Bool r -> Bool.compare l r
    | `Bool _, _ -> -1
    | _, `Bool _ -> 1
    | `Float l, `Float r -> Float.compare l r
    | `Float _, _ -> -1
    | _, `Float _ -> 1
    | `String l, `String r -> String.compare l r
    | `String _, _ -> -1
    | _, `String _ -> 1
    | `A l, `A r -> List.compare compare l r
    | `A _, _ -> -1
    | _, `A _ -> 1
    | `O l, `O r ->
      let compare (l, _) (r, _) = String.compare l r
      and compare_field (lk, lv) (rk, rv) =
        match String.compare lk rk with 0 -> compare lv rv | n -> n
      in
      let l = List.sort ~compare l
      and r = List.sort ~compare r in
      List.compare compare_field l r

  let rec equal l r =
    match (l, r) with
    | `Null, `Null -> true
    | `Bool l, `Bool r -> Bool.equal l r
    | `Float l, `Float r -> Float.equal l r
    | `String l, `String r -> String.equal l r
    | `A l, `A r -> List.equal equal l r
    | `O l, `O r ->
      let compare (l, _) (r, _) = String.compare l r
      and equal (lk, lv) (rk, rv) = String.equal lk rk && equal lv rv in
      let l = List.sort ~compare l
      and r = List.sort ~compare r in
      List.equal equal l r
    | _ -> false
end

module Impl_ezjsonm = struct
  let of_string s =
    match Ezjsonm.value_from_string s with
    | v -> Result.return v
    | exception Ezjsonm.Parse_error (_, msg) -> Result.fail msg

  let to_string t = Ezjsonm.value_to_string ~minify:true t
end

module T = Make (Impl_ezjsonm)
include T
module String = Coding.Make_string (T)

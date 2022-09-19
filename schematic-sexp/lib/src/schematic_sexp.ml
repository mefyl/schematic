open Base
open Schematic
module Sexp = Sexplib.Sexp
include Schematic_sexp_intf

let schema = Schema.make Any
let schema_versioned (_ : Schema.version) = schema

module Sexp_coding : Coding.IMPL with type t = t = struct
  type nonrec t = t

  let pp = Sexplib.Sexp.pp

  include Coding.Defaults

  let type_name = function
    | Sexp.Atom _ -> "incompatible atom"
    | Sexp.List [] -> "empty list"
    | Sexp.List _ -> "incompatible list"

  (* Helpers *)
  let of_atom conv = function
    | Sexp.Atom v -> ( try Some (conv v) with _ -> None)
    | _ -> None

  let to_atom conv v = Sexp.Atom (conv v)

  (* Interface *)
  let null () = Sexp.unit
  let of_bool b = Sexp.Atom (Bool.to_string b)

  let of_fields fields =
    Sexp.List
      (List.map ~f:(fun (name, v) -> Sexp.(List [ Atom name; v ])) fields)

  let of_float = to_atom Float.to_string
  let of_int = to_atom Int.to_string
  let of_int64 = to_atom Int64.to_string
  let of_json = Json_derivers.Jsonm.sexp_of_value
  let of_list l = Sexp.List l
  let of_option = function Some v -> Sexp.List [ v ] | None -> Sexp.List []
  let of_string s = Sexp.Atom s
  let of_time t = Sexp.Atom (Ptime.to_rfc3339 t)

  let of_time_local (t, tz_offset_s) =
    Sexp.Atom (Ptime.to_rfc3339 ~tz_offset_s t)

  let to_bool = of_atom Bool.of_string

  let to_fields = function
    | Sexp.List fields ->
      List.map fields ~f:(function
        | Sexp.(List [ Atom name; value ]) -> Some (name, value)
        | _ -> None)
      |> Option.all
    | _ -> None

  let to_float = of_atom Float.of_string
  let to_int = of_atom Int.of_string
  let to_int64 = of_atom Int64.of_string

  let to_json v =
    try Some (Json_derivers.Jsonm.value_of_sexp v) with _ -> None

  let to_list = function Sexp.List l -> Some l | _ -> None

  let to_option = function
    | Sexp.List [ v ] -> Some (Some v)
    | Sexp.List [] -> Some None
    | _ -> None

  let to_string = function Sexp.Atom s -> Some s | _ -> None

  let to_time_ = function
    | Sexp.Atom s ->
      let error (`RFC3339 (_, e)) =
        Some (Caml.Format.asprintf "%a" Ptime.pp_rfc3339_error e)
      in
      Ptime.of_rfc3339 ~strict:true s |> Result.map_error ~f:error
    | _ -> Result.fail None

  let to_time v =
    let open Result in
    to_time_ v >>| fun (t, _tz, _) -> t

  let to_time_local v =
    let open Result in
    to_time_ v >>= function
    | t, Some tz, _ -> Result.return (t, tz)
    | _, None, _ -> Result.fail (Some "missing timezone")
end

include Coding.Make (Sexp_coding)

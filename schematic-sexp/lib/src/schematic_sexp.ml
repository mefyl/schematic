open Base
open Schematic
module Sexp = Sexplib.Sexp
include Sexp
include Schematic_sexp_intf

type Coding.t += Coding
type Coding.any += Sexp of t

module Sexp_coding : Coding.IMPL with type t = t = struct
  let coding = Coding

  type nonrec t = t

  let name = "s-expression"
  let compare = Sexplib.Sexp.compare
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
  let of_any = function Sexp v -> Some v | _ -> None
  let of_bool b = Sexp.Atom (Bool.to_string b)

  let of_fields fields =
    Sexp.List
      (List.map ~f:(fun (name, v) -> Sexp.(List [ Atom name; v ])) fields)

  let of_float = to_atom Float.to_string
  let of_int = to_atom Int.to_string
  let of_int64 = to_atom Int64.to_string

  (* Shamelessly stolen from json-derivers, without the dependency on an
     obsolete version of Base *)
  let rec of_json = function
    | `Null -> Sexplib.Sexp.Atom "Null"
    | `Bool v0 ->
      Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Bool"; sexp_of_bool v0 ]
    | `Float v0 ->
      Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Float"; sexp_of_float v0 ]
    | `String v0 ->
      Sexplib.Sexp.List [ Sexplib.Sexp.Atom "String"; sexp_of_string v0 ]
    | `A v0 ->
      Sexplib.Sexp.List [ Sexplib.Sexp.Atom "A"; sexp_of_list of_json v0 ]
    | `O v0 ->
      Sexplib.Sexp.List
        [
          Sexplib.Sexp.Atom "O";
          sexp_of_list
            (function
              | v0, v1 ->
                let v0 = sexp_of_string v0
                and v1 = of_json v1 in
                Sexplib.Sexp.List [ v0; v1 ])
            v0;
        ]

  let of_list l = Sexp.List l
  let of_option = function Some v -> Sexp.List [ v ] | None -> Sexp.List []
  let of_string s = Sexp.Atom s
  let of_time t = Sexp.Atom (Ptime.to_rfc3339 t)

  let of_time_local (t, tz_offset_s) =
    Sexp.Atom (Ptime.to_rfc3339 ~tz_offset_s t)

  let to_any = function v -> Sexp v
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

  (* Shamelessly stolen from json-derivers, without the dependency on an
     obsolete version of Base *)
  let rec to_json : t -> Ezjsonm.value option = function
    | Sexplib.Sexp.Atom atom as _sexp -> (
      match atom with
      | "Null" -> Some `Null
      | "Bool" | "Float" | "String" | "A" | "O" | _ -> None)
    | Sexplib.Sexp.List (Sexplib.Sexp.Atom atom :: sexp_args) as _sexp -> (
      try
        match atom with
        | "Bool" as _tag -> (
          match sexp_args with
          | v0 :: [] ->
            let v0 = bool_of_sexp v0 in
            Some (`Bool v0)
          | _ -> None)
        | "Float" as _tag -> (
          match sexp_args with
          | v0 :: [] ->
            let v0 = float_of_sexp v0 in
            Some (`Float v0)
          | _ -> None)
        | "String" as _tag -> (
          match sexp_args with
          | v0 :: [] ->
            let v0 = string_of_sexp v0 in
            Some (`String v0)
          | _ -> None)
        | "A" as _tag -> (
          match sexp_args with
          | v0 :: [] ->
            let v0 = list_of_sexp to_json v0 |> Option.all in
            Option.map ~f:(fun v -> `A v) v0
          | _ -> None)
        | "O" as _tag -> (
          match sexp_args with
          | v0 :: [] ->
            let v0 =
              list_of_sexp
                (function
                  | Sexplib.Sexp.List [ v0; v1 ] ->
                    let v0 = string_of_sexp v0
                    and v1 = to_json v1 in
                    Option.map ~f:(fun v1 -> (v0, v1)) v1
                  | _ -> None)
                v0
              |> Option.all
            in
            Option.map ~f:(fun f -> `O f) v0
          | _ -> None)
        | "Null" -> None
        | _ -> None
      with _ -> None)
    | Sexplib.Sexp.List (Sexplib.Sexp.List _ :: _) -> None
    | Sexplib.Sexp.List [] -> None

  let to_list = function Sexp.List l -> Some l | _ -> None

  let to_option = function
    | Sexp.List [ v ] -> Some (Some v)
    | Sexp.List [] -> Some None
    | _ -> None

  let to_string = function Sexp.Atom s -> Some s | _ -> None

  let to_time_ = function
    | Sexp.Atom s ->
      let error (`RFC3339 (_, e)) =
        Some (Stdlib.Format.asprintf "%a" Ptime.pp_rfc3339_error e)
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
include Coding.Any (Sexp_coding)

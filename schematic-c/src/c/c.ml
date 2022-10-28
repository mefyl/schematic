open Base
include C_intf
include C_schemas
module Runtime = C_runtime
module Builtins = C_builtins
module Swift = Swift
module Typeexpr = C_typeexpr
module Format = C_pp
module Statements = C_statements
module Schemas = C_schemas

let export ?safety_checks (name, (c : c)) : c_gen =
  let open Fmt in
  let of_caml, to_caml = C_conv.converters ?safety_checks name c in
  { name; type_expr = const C_typeexpr.pp c; to_caml; of_caml }

let typedef_src { name; type_expr; _ } =
  let s = Fmt.to_to_string type_expr () in
  Fmt.str "typedef %s %a;\n" s C_identifier.pp name

let statics_src { name; _ } =
  let checking, checked = checking_checked_for name in
  Fmt.str "static bool %s = false;\nstatic bool %s = false;" checking checked

let signature_src { signature; _ } = Fmt.str "%s;" signature

let impl_src { signature; impl } =
  Fmt.to_to_string (C_pp.block ~indent:2 signature impl) ()

let roundtrip_src (name, c) =
  let open Fmt in
  let open C_pp in
  (* Bit of a hack... *)
  let typ =
    let c = `Atom (name, Some c) in
    Fmt.to_to_string C_typeexpr.pp c
  in
  let decl = Fmt.str "CAMLprim value %s(value v)" (roundtrip_for name) in
  let body =
    const pp_statements
      C_statements.
        [
          p1 "v";
          "CAMLlocal1(ret)";
          Fmt.str "%s x = %a(v)" typ Identifier.pp (of_caml_name_for name);
          Fmt.str "ret = %a(x)" Identifier.pp (to_caml_name_for name);
          "CAMLreturn(ret)";
        ]
  in
  let pp = block ~indent:2 decl body in
  Fmt.to_to_string pp ()

let export_ctypes ?(safety_checks = false) ctypes =
  let nl_concat = String.concat ~sep:"\n" in
  let cgens = ctypes |> List.map ~f:(export ~safety_checks) in
  let user_declarations, user_impls =
    let typedefs = cgens |> List.map ~f:typedef_src |> nl_concat in
    let signatures, impls =
      cgens
      |> List.map ~f:(fun { to_caml; of_caml; _ } ->
             ( List.map [ to_caml; of_caml ] ~f:signature_src,
               List.map [ to_caml; of_caml ] ~f:impl_src ))
      |> List.unzip
    in
    let signatures = List.concat signatures |> nl_concat in
    let impls = List.concat impls |> nl_concat in
    ([ typedefs; signatures ] |> nl_concat, impls)
  in
  let header_file_contents = Builtins.declarations ^ user_declarations in
  (* Would be nice to generate signature for the roundtrip functions too, though
     that isn't strictly needed. *)
  let statics =
    if safety_checks then
      let per_type_statics = cgens |> List.map ~f:statics_src |> nl_concat in
      nl_concat
        [
          Fmt.str "static const value* %s = NULL;"
            C_builtins.poly_compare_symbol;
          per_type_statics;
        ]
    else ""
  in
  let rts = ctypes |> List.map ~f:roundtrip_src |> nl_concat in
  (* TODO include declaration file *)
  let c_file_contents =
    [ Builtins.implementations; statics; rts; user_impls ]
    |> String.concat ~sep:"\n\n"
  in
  (header_file_contents, c_file_contents)

let ctypes_of_schemas = of_schemas

(** Returns (header file content) * (implementation file content).

    Please note: the implementation has no `#include "..."` for the header file,
    since its name and location are not yet determined. It must be added by
    consumers of this function. *)
let export_schemas ?(safety_checks = false)
    (schemas : Schematic.Schema.pack list) =
  of_schemas schemas
  |> List.filter_map ~f:(function Some id, c -> Some (id, c) | _ -> None)
  |> export_ctypes ~safety_checks

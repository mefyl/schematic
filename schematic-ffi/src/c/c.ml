open Base
include C_intf
include Ffi_schemas
module Runtime = Runtime
module Builtins = C_builtins
module Swift = Swift
module Typeexpr = C_typeexpr
module Format = C_format
module Statements = C_statements
module Schemas = Ffi_schemas

let export ?safety_checks (name, c) : c_gen =
  let open Fmt in
  let of_caml = C_conv.pp_of_caml_converter ?safety_checks (name, c) in
  let to_caml = C_conv.pp_to_caml_converter (name, c) in
  { name; type_expr = const C_typeexpr.pp c; to_caml; of_caml }

let typedef_src { name; type_expr; _ } =
  let s = Fmt.to_to_string type_expr () in
  Fmt.str "typedef %s %a;\n" s Identifier.pp name

let statics_src { name; _ } =
  let checking, checked = checking_checked_for name in
  Fmt.str "static bool %s = false;\nstatic bool %s = false;" checking checked

let signature_src { signature; _ } = Fmt.str "%s;" signature

let impl_src { signature; impl } =
  Fmt.to_to_string (C_format.block ~indent:2 signature impl) ()

let roundtrip_src name =
  let open Fmt in
  let open C_format in
  let decl = Fmt.str "CAMLprim value %s(value v)" (roundtrip_for name) in
  let body =
    const pp_statements
      C_statements.
        [
          p1 "v";
          "CAMLlocal1(ret)";
          Fmt.str "%a x = %a(v)" Identifier.pp name Identifier.pp
            (of_caml_name_for name);
          Fmt.str "ret = %a(x)" Identifier.pp (to_caml_name_for name);
          "CAMLreturn(ret)";
        ]
  in
  let pp = block ~indent:2 decl body in
  Fmt.to_to_string pp ()

let ctypes_to_h_and_c ?(safety_checks = false)
    (ctypes : (Identifier.t * c) list) : string * string =
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
  let rts =
    ctypes |> List.map ~f:fst |> List.map ~f:roundtrip_src |> nl_concat
  in
  (* TODO include declaration file *)
  let c_file_contents =
    [ Builtins.implementations; statics; rts; user_impls ]
    |> String.concat ~sep:"\n\n"
  in
  (header_file_contents, c_file_contents)

let schemas_to_h_and_c ?(safety_checks = false)
    (schemas : Schematic.Schema.pack list) =
  ctypes_of_schemas schemas
  (* |> List.map ~f:(fun c -> (Fmt.epr ""; c) *)
  |> ctypes_to_h_and_c ~safety_checks

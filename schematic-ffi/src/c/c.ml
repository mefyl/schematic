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

let nl_concat = String.concat ~sep:"\n"

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

let h_and_c_of_ctypes ?(safety_checks = false) ~h_include
    (ctypes : (Identifier.t * c) list) : string * string =
  let cgens = ctypes |> List.map ~f:(export ~safety_checks) in
  let user_declarations, user_impls =
    let typedefs = cgens |> List.map ~f:typedef_src in
    let signatures, impls =
      cgens
      |> List.map ~f:(fun { to_caml; of_caml; _ } ->
             let fns = [ to_caml; of_caml ] in
             (List.map fns ~f:signature_src, List.map fns ~f:impl_src))
      |> List.unzip
    in
    let signatures = List.concat signatures in
    let impls = List.concat impls in
    (typedefs @ signatures, impls)
  in
  (* TODO include guards *)
  let header_file_contents =
    Builtins.declarations :: user_declarations |> nl_concat
  in
  let statics =
    [ Fmt.str "static const value* %s = NULL;" C_builtins.poly_compare_symbol ]
    @ (cgens |> List.map ~f:statics_src)
    |> Option.some_if safety_checks
    |> Option.to_list |> List.concat
  in
  let c_file_contents =
    Fmt.str "#include \"%s\"" h_include
    :: Builtins.implementations :: (statics @ user_impls)
    |> nl_concat
  in
  (header_file_contents, c_file_contents)

let h_and_c_of_schemas ?(safety_checks = false)
    (schemas : Schematic.Schema.pack list) =
  ctypes_of_schemas schemas |> h_and_c_of_ctypes ~safety_checks

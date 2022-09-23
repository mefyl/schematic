open Base
include C_intf

(* End of shameless copy *)

(* XXX: Butchering the boxing logic while I learn it. *)

include C_schemas
module Runtime = C_runtime
module Builtins = C_builtins

let export (name, c) : c_gen =
  let open Fmt in
  let of_caml, to_caml = C_conv.converters name c in
  { name; type_expr = const C_typeexpr.pp c; to_caml; of_caml }

let typedef_src { name; type_expr; _ } =
  let s = Fmt.to_to_string type_expr () in
  Fmt.str "typedef %s %a;\n" s C_identifier.pp name

let statics_src { name; _ } =
  let checking, checked = checking_checked_for name in
  Fmt.str "static bool %s = false;\nstatic bool %s = false;" checking checked

let signature_src { signature; _ } = Fmt.str "%s;" signature

let impl_src { signature; impl } =
  Fmt.to_to_string (C_pp.braced ~indent:2 signature impl) ()

let free_src (_ : c_gen) = failwith "no free() yet"

let roundtrip_src (name, c) =
  let open Fmt in
  let open C_pp in
  (* Bit of a hack... *)
  let c = Atom (name, Some c) in
  let typ = Fmt.to_to_string C_typeexpr.pp c in

  let decl = Fmt.str "CAMLprim value %s(value v)" (roundtrip_for name) in
  let body =
    const pp_statements
      [
        "CAMLparam1(v)";
        "CAMLlocal1(ret)";
        Fmt.str "%s x = %a(v)" typ Identifier.pp (of_caml_name_for name);
        Fmt.str "ret = %a(x)" Identifier.pp (to_caml_name_for name);
        "CAMLreturn(ret)";
      ]
  in
  let pp = braced ~indent:2 decl body in
  Fmt.to_to_string pp ()

(* let runtime = C_runtime.runtime

   (** You MUST call this if you want support for arbitrary JSON values. *) let
   register_callbacks = C_runtime.register_callbacks *)

(** Returns (header file content) * (implementation file content)

    XXX: need to include header in impl! *)

(* TODO: static for unexported symbols? *)
let export_schemas ?(safety_checks = false) (schemas : Schema.pack list) =
  let nl_concat = String.concat ~sep:"\n" in

  let ctypes = of_schemas_state state_empty schemas |> fst |> List.rev in

  let cgens = ctypes |> List.map ~f:export in

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

  (* TODO: gen roundtrip signatures too *)
  let statics =
    if safety_checks then cgens |> List.map ~f:statics_src |> nl_concat else ""
  in
  let rts = ctypes |> List.map ~f:roundtrip_src |> nl_concat in

  (* TODO include declaration file *)
  let c_file_contents =
    [ Builtins.implementations; statics; rts; user_impls ]
    |> String.concat ~sep:"\n\n"
  in

  (header_file_contents, c_file_contents)

(* let () = Fmt.pr "%s\n" Schematic.C.runtime; Fmt.pr "\n"; Fmt.pr "//\n";
   Fmt.pr "// Generated declarations\n"; Fmt.pr "//\n"; Fmt.pr "%s"
   declarations; Fmt.pr "\n"; Fmt.pr "//\n"; Fmt.pr "// Generated
   implementations\n"; Fmt.pr "//\n"; Fmt.pr "%s" impls; Fmt.pr "\n"; Fmt.pr
   "\n"; Fmt.pr "%s" rts; () *)

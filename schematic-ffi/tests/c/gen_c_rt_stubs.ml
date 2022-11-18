
open Base
open Ffi_test_utils.Types

let roundtrip_for = Fmt.str "sch_%a_rt_id" Schematic_ffi.C.Identifier.pp

let roundtrip_src name =
  let open Fmt in
  let open Schematic_ffi.C.Format in
  let decl = Fmt.str "CAMLprim value %s(value v)" (roundtrip_for name) in
  let body =
    const pp_statements
      Schematic_ffi.C.(
        Statements.
          [
            p1 "v";
            "CAMLlocal1(ret)";
            Fmt.str "%a x = %a(v)" Identifier.pp name Identifier.pp
              (of_caml_name_for name);
            Fmt.str "x = sch_%a_swift_rt_id(x)" Identifier.pp name;
            Fmt.str "ret = %a(x)" Identifier.pp (to_caml_name_for name);
            "CAMLreturn(ret)";
          ])
  in
  let pp =
    Schematic_ffi.C.(
      const string
        (Fmt.str "%a sch_%a_swift_rt_id(%a);\n" Identifier.pp name Identifier.pp
           name Identifier.pp name))
    ++ block ~indent:2 decl body
  in
  Fmt.to_to_string pp ()

(* Would be nice to generate signature for the roundtrip functions too, though
   that isn't strictly needed. *)
let roundtrips_of_ctypes ctypes =
  ctypes |> List.map ~f:fst |> List.map ~f:roundtrip_src |> String.concat

let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  let argv = Sys.get_argv () in
  let h_filename, c_filename = (argv.(1), argv.(2)) in
  let ctypes = Schematic_ffi.ctypes_of_schemas exported_schemas in
  let h_contents, c_contents =
    Schematic_ffi.C.h_and_c_of_ctypes ~safety_checks:false ~h_include:h_filename
      ctypes
  in
  let roundtrip_impls = roundtrips_of_ctypes ctypes in
  let open Stdio.Out_channel in
  write_all h_filename ~data:h_contents;
  write_all c_filename ~data:(c_contents ^ roundtrip_impls)

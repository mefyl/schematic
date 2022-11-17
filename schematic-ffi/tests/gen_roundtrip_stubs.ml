open Base
open Ffi_test_types

let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  let argv = Sys.get_argv () in
  let h_filename, c_filename = (argv.(1), argv.(2)) in
  let h_contents, c_contents =
    Schematic_ffi.C.schemas_to_h_and_c ~safety_checks:true ~h_include:h_filename exported_schemas
  in
  let open Stdio.Out_channel in
  write_all h_filename ~data:h_contents;
  write_all c_filename ~data:c_contents

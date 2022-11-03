open Base
open C_test_types
open Schematic_ffi

let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  let argv = Sys.get_argv () in
  let h_filename, c_filename = (argv.(1), argv.(2)) in
  let h_contents, c_contents =
    C.export_schemas ~safety_checks:false exported_schemas
  in
  let c_contents = Fmt.str "#include \"%s\"" h_filename ^ c_contents in
  let open Stdio.Out_channel in
  write_all h_filename ~data:h_contents;
  write_all c_filename ~data:c_contents

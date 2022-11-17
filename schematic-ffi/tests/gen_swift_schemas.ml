open Base
open C_test_types
open Schematic_ffi

let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  let argv = Sys.get_argv () in
  let swift_filename = argv.(1) in
  Logs.info (fun m -> m "Regenerating %s" swift_filename);
  let swift_contents = Swift.export_schemas exported_schemas in
  let open Stdio.Out_channel in
  write_all swift_filename ~data:swift_contents

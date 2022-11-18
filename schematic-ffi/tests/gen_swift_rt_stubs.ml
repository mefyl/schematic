open Base
open Ffi_test_utils.Types
open Schematic_ffi

let roundtrip_fn_for (id, c) =
  let c = `Atom (id, Some c) in
  let ct = Swift.Swiftify.C_type.c c in
  let st = Fmt.to_to_string Swift.Typeexpr.pp_swift_type c in
  let fn = Fmt.str "%s_rt_id" ct in
  Fmt.str
    {|
@_cdecl("sch_%s")
func %s(_ x: %s) -> %s {
    let sw: %s = schematicOfC(x)
    return schematicToC(sw)
}
|}
    fn fn ct ct st

let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  let argv = Sys.get_argv () in
  let swift_filename = argv.(1) in
  Logs.info (fun m -> m "Regenerating %s" swift_filename);
  Swift.c_module_name := None;
  let ctypes = ctypes_of_schemas exported_schemas in
  let schemas_src = Swift.export_ctypes ctypes in
  let rts_src =
    ctypes |> List.map ~f:roundtrip_fn_for |> String.concat ~sep:"\n"
  in
  let swift_contents = schemas_src ^ rts_src in

  let open Stdio.Out_channel in
  write_all swift_filename ~data:swift_contents

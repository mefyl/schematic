open Base
open Ffi_test_utils.Types
open Schematic_ffi

let roundtrip_fn_for (id, c) =
  let c = `Atom (id, Some c) in
  let ct = Swift.Swiftify.C_type.c c in
  let st = Fmt.to_to_string Swift.Typeexpr.pp_swift_type c in
  let fn = Fmt.str "%s_swift_rt_id" ct in
  Fmt.str
    {|
@_cdecl("sch_%s")
public func %s(_ x: %s) -> %s {
    //print("C obj: ", x as Any)
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
  let swift_contents = {|import CSchemas
|} ^ schemas_src ^ rts_src in

  Stdlib.Out_channel.(
    with_open_text swift_filename @@ fun chan ->
    output_string chan swift_contents)

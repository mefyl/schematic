open Base

(** Crawl all schemas from the test suite *)
let test_schemas : Schematic.Schema.pack list =
  let ( let* ) = List.( >>= ) in
  let ( let+ ) = List.( >>| ) in

  let* { test_cases; _ } = Swift_utils.Tests.test_descriptions in
  let* { tests; _ } = test_cases in
  let* (Test { schema; is_swift_roundtrip_enabled; _ }) = tests in
  let+ schema = if is_swift_roundtrip_enabled then [ schema ] else [] in
  Schematic.Schema.pack schema

(** Generate the test Swift file with some test data types *)
let () =
  let () = Logs.set_reporter (Logs_fmt.reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let swift_types = Schematic.Swift.of_packed_schemas test_schemas in
  Fmt.pr {|/**
 * Generated from OCaml types, do not edit manually
 */

%a
|}
    Schematic.Swift.pp_file
    (* The [All] type is a union of all the known types.

       It is required because the tests want to dynamically choose a type based
       on the command-line arguments, and Swift doesn't have this kind of
       reflexion capability. This big union gives us back just enough to
       implement it. *)
    (swift_types @ [ Schematic.Swift.union_of swift_types "All" ])

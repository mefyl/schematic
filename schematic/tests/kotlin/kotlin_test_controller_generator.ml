(** Generate the test Kotlin file with some test data types *)
let () =
  let schemas = [ Schematic.Schema.pack Test_utils.Tests.foo_schema ] in
  let kotlin_types = Schematic.Kotlin.of_packed_schemas schemas in
  Fmt.pr {|/**
 * Generated from OCaml types, do not edit manually
 */

%a
|}
    (Schematic.Kotlin.pp_file ~package:"co.routine")
    kotlin_types

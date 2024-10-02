open Base

(** Crawl all schemas from the test suite *)
let test_schemas : Schematic.Schema.pack list =
  let ( let* ) = List.( >>= ) in
  let ( let+ ) = List.( >>| ) in

  let* { test_cases; _ } = Kotlin_utils.Tests.test_descriptions in
  let* { tests; _ } = test_cases in
  let+ (Test { schema; _ }) = tests in
  Schematic.Schema.pack schema

(** Generate the test Kotlin file with some test data types *)
let () =
  let kotlin_types = Schematic.Kotlin.of_packed_schemas test_schemas in
  Fmt.pr {|/**
 * Generated from OCaml types, do not edit manually
 */

%a
|}
    (Schematic.Kotlin.pp_file ~package:"co.routine")
    kotlin_types

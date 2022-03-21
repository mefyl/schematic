module B64 = Schematic.Base64.Make (Schematic.Json.String)

let decoding_error =
  Alcotest.testable Schematic.Error.pp_decoding Schematic.Error.equal_decoding

let roundtrip test schema v repr =
  let encoded = B64.encode schema v in
  let () = Alcotest.(check string "encoded" repr encoded) in
  let decoded = B64.decode schema encoded in
  Alcotest.(check (result test decoding_error) "decoded" (Result.Ok v) decoded)

let null () =
  roundtrip Alcotest.unit Schematic.Schemas.unit_schema () "bnVsbA=="

let number () =
  roundtrip Alcotest.int Schematic.Schemas.int_schema 20220316 "MjAyMjAzMTY="

let () =
  let open Alcotest in
  let test_case name f = test_case name `Quick f in
  run "Schematic.Base64"
    [ ("coding", [ test_case "null" null; test_case "number" number ]) ]

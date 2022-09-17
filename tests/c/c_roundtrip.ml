open C_test_types

external wrapper_rt_id : wrapper -> wrapper = "sch_wrapper_rt_id"
external diverse_rt_id : diverse -> diverse = "sch_diverse_rt_id"

external basic_variant_rt_id : basic_variant -> basic_variant
  = "sch_basic_variant_rt_id"

external payload_variant_rt_id : payload_variant -> payload_variant
  = "sch_payload_variant_rt_id"

(* TODO variants*)

(* Most robust way of testing conversion, for sure:

   Converts values from Caml to C, then back to Caml, and check equality.

   To do so, have a first test suite step that generates the C file. This C file
   will expose primitives (or just one? XXX).

   It is then compiled. Finally, an alcotest binary will be LINKED with the
   generated object files, and call for each type a test primitive that takes an
   OCaml value, makes it jump through hoops, then returns it.

   Then it checks for equality. *)

module Tests = struct
  let test t got expected = Alcotest.(check t "TODO" expected) got

  let wrapper () =
    let x = { somevar = 42; r = { a = 1; b = 77 } } in
    let new_x = wrapper_rt_id x in
    let t = Alcotest.testable pp_wrapper equal_wrapper in
    test t new_x x

  let diverse () =
    let x = { s = "Hello world!" } in
    let new_x = diverse_rt_id x in
    let t = Alcotest.testable pp_diverse equal_diverse in
    test t new_x x

  let basic_variant () =
    let x = B in
    let new_x = basic_variant_rt_id x in
    let t = Alcotest.testable pp_basic_variant equal_basic_variant in
    test t new_x x

  let payload_variant_plain () =
    let x = Nothing2 in
    let new_x = payload_variant_rt_id x in
    let t = Alcotest.testable pp_payload_variant equal_payload_variant in
    test t new_x x

  let payload_variant_with_pl () =
    let x = SomeInt 53 in
    let new_x = payload_variant_rt_id x in
    let t = Alcotest.testable pp_payload_variant equal_payload_variant in
    test t new_x x
end

let () =
  let open Alcotest in
  run "Schematic.C roundtrip"
    [
      ( "typegen",
        Tests.
          [
            test_case "wrapper" `Quick wrapper;
            test_case "diverse" `Quick diverse;
            test_case "basic_variant" `Quick basic_variant;
            test_case "payload_variant: plain" `Quick payload_variant_plain;
            test_case "payload_variant: with payload" `Quick
              payload_variant_with_pl;
          ] );
    ]

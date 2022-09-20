open C_test_types

external wrapper_rt_id : wrapper -> wrapper = "sch_wrapper_rt_id"
external diverse_rt_id : diverse -> diverse = "sch_diverse_rt_id"

external basic_variant_rt_id : basic_variant -> basic_variant
  = "sch_basic_variant_rt_id"

external payload_variant_rt_id : payload_variant -> payload_variant
  = "sch_payload_variant_rt_id"

external behemoth_rt_id : behemoth -> behemoth = "sch_behemoth_rt_id"
external collection_rt_id : collection -> collection = "sch_collection_rt_id"

external int64_alias_rt_id : int64_alias -> int64_alias
  = "sch_int64_alias_rt_id"

external bool_alias_rt_id : bool_alias -> bool_alias = "sch_bool_alias_rt_id"

external float_alias_rt_id : float_alias -> float_alias
  = "sch_float_alias_rt_id"

(* Most robust way of testing conversion, for sure:

   Converts values from Caml to C, then back to Caml, and check equality.

   To do so, have a first test suite step that generates the C file. This C file
   will expose primitives (or just one? XXX).

   It is then compiled. Finally, an alcotest binary will be LINKED with the
   generated object files, and call for each type a test primitive that takes an
   OCaml value, makes it jump through hoops, then returns it.

   Then it checks for equality. *)

module Tests = struct
  let test t got expected = Alcotest.(check t "" expected) got

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

  let payload_variant_with_inline_tuple_payload () =
    let x = A_pair (990, "cocorico") in
    let new_x = payload_variant_rt_id x in
    let t = Alcotest.testable pp_payload_variant equal_payload_variant in
    test t new_x x

  let behemoth () =
    let x : behemoth =
      {
        d = { s = "Coucou" };
        b = B;
        p = SomeInt 101;
        pair = ("hello", { somevar = 1; r = { a = 0; b = 56000 } });
        pair_alias = ("coin", 8);
      }
    in
    let new_x = behemoth_rt_id x in
    let t = Alcotest.testable pp_behemoth equal_behemoth in
    test t new_x x

  let collection () =
    let x = [ 12; 953878; 1; 0; 0; 3; -43 ] in
    let new_x = collection_rt_id x in
    let t = Alcotest.testable pp_collection equal_collection in
    test t new_x x

  let int64 () =
    let x : int64 = 8000000000L in
    let new_x = int64_alias_rt_id x in
    let t = Alcotest.testable pp_int64_alias equal_int64_alias in
    test t new_x x

  let bool () =
    let t = Alcotest.testable pp_bool_alias equal_bool_alias in
    [ true; false ]
    |> List.iter (fun x ->
           let new_x = bool_alias_rt_id x in
           test t new_x x)

  let float () =
    let t = Alcotest.testable pp_float_alias equal_float_alias in
    (* Reminder: nan != nan *)
    [ -1343.; 0.; 1.; 1e65; 1e300 ]
    |> List.iter (fun x ->
           let new_x = float_alias_rt_id x in
           test t new_x x)
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
            test_case "payload_variant: payload" `Quick payload_variant_with_pl;
            test_case "payload_variant: inline tuple payload" `Quick
              payload_variant_with_inline_tuple_payload;
            test_case "behemoth" `Quick behemoth;
            test_case "collection" `Quick collection;
            test_case "int64" `Quick int64;
            test_case "bool" `Quick bool;
            test_case "float" `Quick float;
          ] );
    ]

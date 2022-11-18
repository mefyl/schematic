open Schematic_ffi
open Ffi_test_utils.Types

[%%metadef
let to_rt_external c_name ct =
  let int_rt_name = c_name ^ "_rt_id" in
  let ext_rt_name = "sch_" ^ c_name ^ "_rt_id" in
  let open Ppxlib in
  let fn_typ : core_type =
    {
      ptyp_desc = Ptyp_arrow (Nolabel, ct, ct);
      ptyp_loc = Location.none;
      ptyp_loc_stack = [];
      ptyp_attributes = [];
    }
  in
  let structure_item : Ppxlib.structure_item =
    {
      pstr_loc = Location.none;
      pstr_desc =
        Pstr_primitive
          {
            pval_name = Metapp_preutils.mkloc int_rt_name;
            pval_loc = Location.none;
            pval_type = fn_typ;
            pval_prim = [ ext_rt_name ];
            pval_attributes = [];
          };
    }
  in
  structure_item]

(* A first step (outside of this file) generates a C file exposing primitives
   that convert values from Caml to C, then back to Caml (a "roundtrip").

   This module is linked with those primitives. It calls them for a variety of
   types and ensures that values are preserved and not trigger assertions.

   Sadly enough, OCaml will not warn us if we exposed a primitive involving a
   type that itself contains a type whose boxed/unboxed status is ambiguous.
   This is dangerous.

   For now (4.14.0) it will work by default, but this may break in the future.
   [@@boxed] will help. *)

[%%meta to_rt_external "wrapper" [%t: wrapper]]
[%%meta to_rt_external "diverse" [%t: diverse]]
[%%meta to_rt_external "behemoth" [%t: behemoth]]
[%%meta to_rt_external "collection" [%t: collection]]
[%%meta to_rt_external "basic_variant" [%t: basic_variant]]
[%%meta to_rt_external "payload_variant" [%t: payload_variant]]
[%%meta to_rt_external "int64_alias" [%t: int64_alias]]
[%%meta to_rt_external "bool_alias" [%t: bool_alias]]
[%%meta to_rt_external "float_alias" [%t: float_alias]]
[%%meta to_rt_external "int_option_alias" [%t: int_option_alias]]
[%%meta to_rt_external "float_map_alias" [%t: float_map_alias]]
[%%meta to_rt_external "record_with_indir_field" [%t: record_with_indir_field]]

[%%meta
to_rt_external "record_containing_ambiguous_record"
  [%t: record_containing_ambiguous_record]]

[%%meta to_rt_external "any_alias" [%t: any_alias]]
[%%meta to_rt_external "time_alias" [%t: time_alias]]
[%%meta to_rt_external "time_local_alias" [%t: time_local_alias]]
[%%meta to_rt_external "date_alias" [%t: date_alias]]
[%%meta to_rt_external "record_with_unit" [%t: record_with_unit]]

[%%meta
to_rt_external "variant_with_inline_record_payload"
  [%t: variant_with_inline_record_payload]]

[%%meta
to_rt_external "payload_variant_with_union_key"
  [%t: payload_variant_with_union_key]]

[%%meta to_rt_external "string_option_record" [%t: string_option_record]]
[%%meta to_rt_external "tuple_int_string_alias" [%t: tuple_int_string_alias]]
[%%meta to_rt_external "poly_basic" [%t: poly_basic]]
[%%meta to_rt_external "poly_payload" [%t: poly_payload]]
[%%meta to_rt_external "evil_defaults" [%t: evil_defaults]]

[%%meta
to_rt_external "string_wrapper_parametric_record"
  [%t: string_wrapper parametric_record]]

[%%meta
to_rt_external "string_wrapper_parametric_record_ultra_generic"
  [%t: reified_ultra_generic]]

[%%meta to_rt_external "with_computed" [%t: with_computed]]
[%%meta to_rt_external "mapped" [%t: mapped]]
[%%meta to_rt_external "event_1" [%t: Event_1.t]]

module Tests_ocaml_c = struct
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

  let tuple_int_string_alias () =
    let t =
      Alcotest.testable pp_tuple_int_string_alias equal_tuple_int_string_alias
    in
    [ (214432, "zdqzd"); (-2443, "") ]
    |> List.iter (fun x ->
           let new_x = tuple_int_string_alias_rt_id x in
           test t new_x x)

  let payload_variant_plain () =
    let x = Nothing3 in
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

  let payload_variant_with_inline_record_payload () =
    let x = Inline_payload { field = 6298 } in
    let new_x = variant_with_inline_record_payload_rt_id x in
    let t =
      Alcotest.testable pp_variant_with_inline_record_payload
        equal_variant_with_inline_record_payload
    in
    test t new_x x

  let payload_variant_with_union_key () =
    let t =
      Alcotest.testable pp_payload_variant_with_union_key
        equal_payload_variant_with_union_key
    in
    (* Reminder: nan != nan *)
    [ Nothing_uk; A_pair_uk (532, -4433); Nothing2_uk ]
    |> List.iter (fun x ->
           let new_x = payload_variant_with_union_key_rt_id x in
           test t new_x x)

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

  let int_option () =
    let t = Alcotest.testable pp_int_option_alias equal_int_option_alias in
    [ None; Some 0; Some (-1); Some 1 ]
    |> List.iter (fun x ->
           let new_x = int_option_alias_rt_id x in
           test t new_x x)

  let string_option_record () =
    let t =
      Alcotest.testable pp_string_option_record equal_string_option_record
    in
    [ { maybe = None }; { maybe = Some "hello" }; { maybe = Some "coin" } ]
    |> List.iter (fun x ->
           let new_x = string_option_record_rt_id x in
           test t new_x x)

  let mapping () =
    let x = [ ("somekey", 54.); ("testy testo", 0.) ] in
    let new_x = float_map_alias_rt_id x in
    let t = Alcotest.testable pp_float_map_alias equal_float_map_alias in
    test t new_x x

  let indirection () =
    let x = { indir = { a = -2; b = 43 } } in
    let new_x = record_with_indir_field_rt_id x in
    let t =
      Alcotest.testable pp_record_with_indir_field equal_record_with_indir_field
    in
    test t new_x x

  let any () =
    (* [ `Null | `Bool of bool | `Float of float | `String of string | `A of
       value list | `O of (string * value) list ] *)
    let x : any_alias =
      `A
        [ `A []; `Float 65.; `O [ ("mykey", `Bool true); ("otherKey", `Null) ] ]
    in
    let new_x = any_alias_rt_id x in
    let t = Alcotest.testable pp_any_alias equal_any_alias in
    test t new_x x

  let time () =
    let t = Alcotest.testable pp_time_alias equal_time_alias in
    [ Ptime.min; Ptime.max ]
    |> List.iter (fun x ->
           let new_x = time_alias_rt_id x in
           test t new_x x)

  let time_local () =
    let t = Alcotest.testable pp_time_local_alias equal_time_local_alias in
    [ (Ptime.min, 0); (Ptime.max, 5443) ]
    |> List.iter (fun x ->
           let new_x = time_local_alias_rt_id x in
           test t new_x x)

  let date () =
    let t = Alcotest.testable (Fmt.any "") equal_date_alias in
    [ (2022, 01, 01); (1970, 01, 01) ]
    |> List.iter (fun x ->
           let new_x = date_alias_rt_id x in
           test t new_x x)

  let record_with_unit () =
    let t = Alcotest.testable pp_record_with_unit equal_record_with_unit in
    [ { concrete = 24322443; nothing = (); after_unit = 4 } ]
    |> List.iter (fun x ->
           let new_x = record_with_unit_rt_id x in
           test t new_x x)

  let reified_parametric () =
    let x = { parameterized_field = "Helloooooo!" } in
    let new_x = string_wrapper_parametric_record_rt_id x in
    let t =
      Alcotest.testable
        (pp_parametric_record pp_string_wrapper)
        (equal_parametric_record equal_string_wrapper)
    in
    test t new_x x

  let poly_basic () =
    let t = Alcotest.testable pp_poly_basic equal_poly_basic in
    [ `A; `B; `C ]
    |> List.iter (fun x ->
           let new_x = poly_basic_rt_id x in
           test t new_x x)

  let poly_payload () =
    let t = Alcotest.testable pp_poly_payload equal_poly_payload in
    [ `A; `B; `Single 532442; `Tuple (213, ":)") ]
    |> List.iter (fun x ->
           let new_x = poly_payload_rt_id x in
           test t new_x x)

  let evil_defaults () =
    let t = Alcotest.testable pp_evil_defaults equal_evil_defaults in
    [ { not_option = -394 }; make_evil_defaults () ]
    |> List.iter (fun x ->
           let new_x = evil_defaults_rt_id x in
           test t new_x x)

  let ultra_generic () =
    let x : reified_ultra_generic =
      {
        (* Fully parameterized field *)
        field = -432;
        (* Partially parameterized field, with a builtin *)
        list = [ { parameterized_field = "cocorico" } ];
        (* Partially parameterized field, with a user-defined type *)
        nested_generic = { parameterized_field = "Coin" };
      }
    in
    let new_x = string_wrapper_parametric_record_ultra_generic_rt_id x in
    let pp_reified_param = pp_parametric_record pp_string_wrapper in
    let equal_reified_param = equal_parametric_record equal_string_wrapper in
    let open Base in
    let t =
      Alcotest.testable
        (pp_ultra_generic Int.pp pp_reified_param pp_string_wrapper)
        (equal_ultra_generic Int.equal equal_reified_param equal_string_wrapper)
    in
    test t new_x x

  let with_computed () =
    let t = Alcotest.testable pp_with_computed equal_with_computed in
    [ { non_computed = 4243 } ]
    |> List.iter (fun x ->
           let new_x = with_computed_rt_id x in
           test t new_x x)

  let encode_decode () =
    let t = Alcotest.testable pp_mapped equal_mapped in
    ([ B; A ] : mapped list)
    |> List.iter (fun x ->
           let new_x = mapped_rt_id x in
           test t new_x x)

  let event_1 () =
    let open Event_1 in
    let t = Alcotest.testable pp equal in
    ([ { wrapped = `B }; { wrapped = `A } ] : t list)
    |> List.iter (fun (x : Event_1.t) ->
           let new_x = event_1_rt_id x in
           test t new_x x)
end

module Tests_ocaml_c_swift = struct end

let () =
  let open Alcotest in
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  C.Runtime.register_static_callbacks ();
  C.Runtime.register_schema mapped_schema;
  C.Runtime.register_schema Event_1.schema;
  run "Schematic_ffi roundtrips" ~verbose:true
    [
      ( "OCaml <-> C",
        Tests_ocaml_c.
          [
            test_case "wrapper" `Quick wrapper;
            test_case "diverse" `Quick diverse;
            test_case "basic_variant" `Quick basic_variant;
            test_case "payload_variant: plain" `Quick payload_variant_plain;
            test_case "payload_variant: payload" `Quick payload_variant_with_pl;
            test_case "payload_variant: inline tuple payload" `Quick
              payload_variant_with_inline_tuple_payload;
            test_case "payload_variant_with_union_key" `Quick
              payload_variant_with_union_key;
            test_case "behemoth" `Quick behemoth;
            test_case "collection" `Quick collection;
            test_case "int64" `Quick int64;
            test_case "bool" `Quick bool;
            test_case "float" `Quick float;
            test_case "int option" `Quick int_option;
            test_case "mapping" `Quick mapping;
            test_case "type alias indirection" `Quick indirection;
            test_case "any (JSON)" `Quick any;
            test_case "time" `Quick time;
            test_case "time_local" `Quick time_local;
            test_case "date" `Quick date;
            test_case "record containing a `unit`" `Quick record_with_unit;
            test_case "payload_variant: inline record payload" `Quick
              payload_variant_with_inline_record_payload;
            test_case "reified parametric record" `Quick reified_parametric;
            test_case "string option record (Todo bug)" `Quick
              string_option_record;
            test_case "basic polymorphic variant" `Quick poly_basic;
            test_case "payload polymorphic variant" `Quick poly_payload;
            test_case "evil defaults" `Quick evil_defaults;
            test_case "ultra generic" `Quick ultra_generic;
            test_case "with computed" `Quick with_computed;
            test_case "tuple_int_string_alias" `Quick tuple_int_string_alias;
            test_case "encode / decode" `Quick encode_decode;
            test_case "event 1" `Quick event_1;
          ] );
      ("OCaml -> C -> Swift and back", Tests_ocaml_c_swift.[]);
    ]

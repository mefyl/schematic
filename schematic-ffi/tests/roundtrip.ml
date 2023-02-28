open Schematic_ffi
open Ffi_test_utils.Types

(* A first step (outside of this file) generates a C file exposing primitives
   that convert values from Caml to C, then back to Caml (a "roundtrip").

   This module is linked with those primitives. It calls them for a variety of
   types and ensures that values are preserved and not trigger assertions.

   Sadly enough, OCaml will not warn us if we exposed a primitive involving a
   type that itself contains a type whose boxed/unboxed status is ambiguous.
   This is dangerous. XXX.

   For now (4.14.0) it will work by default, but this may break in the future.
   [@@boxed] will help. *)

[%%metadef
let to_rt_external c_name suff ct =
  let int_rt_name = c_name ^ suff ^ "_rt_id" in
  let ext_rt_name = "sch_" ^ c_name ^ suff ^ "_rt_id" in
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
  structure_item

let to_rt_external c_name typ = to_rt_external c_name "" typ]

[%%metadef
let test_case name core_type rt_expr pp_expr eq_expr test_expr =
  let expr =
    [%e
      let open struct
        [%%meta to_rt_external name core_type]
      end in
      let test t got expected = Alcotest.(check t "" expected) got in
      let t = Alcotest.testable [%meta pp_expr] [%meta eq_expr] in
      [%meta test_expr]
      |> List.iter (fun x ->
             let new_x = [%meta rt_expr] x in
             test t new_x x)]
  in
  let open Ppxlib in
  let f = [%e fun () -> [%meta expr]] in
  let name = Metapp_preutils.Exp.of_string name in
  [%e test_case [%meta name] `Quick [%meta f]]

let to_test_cases l =
  let l =
    List.map
      (fun (c_name, core_type, rt, pp, equal, cases) ->
        test_case c_name core_type rt pp equal cases)
      l
  in
  Metapp_preutils.Exp.list l]

let () =
  let open Alcotest in
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  C.Runtime.register_static_callbacks ();

  (* Schemas using dynamic components must be registered *)
  C.Runtime.register_schema mapped_schema;
  C.Runtime.register_schema Event_1.schema;

  let ptime_sample =
    let dt = ((2022, 11, 04), ((13, 12, 3), 0)) in
    Ptime.of_date_time dt |> Base.Option.value_exn
  in
  run "Schematic_ffi roundtrips" ~verbose:true
    [
      ( "",
        [%meta
          to_test_cases
            [
              ( "wrapper",
                [%t: wrapper],
                [%e wrapper_rt_id],
                [%e pp_wrapper],
                [%e equal_wrapper],
                [%e [ { somevar = 42; r = { a = 1; b = 77 } } ]] );
              ( "diverse",
                [%t: diverse],
                [%e diverse_rt_id],
                [%e pp_diverse],
                [%e equal_diverse],
                [%e [ { s = "Hello world!" } ]] );
              ( "basic_variant",
                [%t: basic_variant],
                [%e basic_variant_rt_id],
                [%e pp_basic_variant],
                [%e equal_basic_variant],
                [%e [ A; B ]] );
              ( "tuple_int_string_alias",
                [%t: tuple_int_string_alias],
                [%e tuple_int_string_alias_rt_id],
                [%e pp_tuple_int_string_alias],
                [%e equal_tuple_int_string_alias],
                [%e [ (214432, "zdqzd"); (-2443, "") ]] );
              ( "payload_variant",
                [%t: payload_variant],
                [%e payload_variant_rt_id],
                [%e pp_payload_variant],
                [%e equal_payload_variant],
                [%e [ Nothing3; SomeInt 53; A_pair (990, "cocorico") ]] );
              ( "variant_with_inline_record_payload",
                [%t: variant_with_inline_record_payload],
                [%e variant_with_inline_record_payload_rt_id],
                [%e pp_variant_with_inline_record_payload],
                [%e equal_variant_with_inline_record_payload],
                [%e [ Inline_payload { field = 6298 } ]] );
              ( "payload_variant_with_union_key",
                [%t: payload_variant_with_union_key],
                [%e payload_variant_with_union_key_rt_id],
                [%e pp_payload_variant_with_union_key],
                [%e equal_payload_variant_with_union_key],
                [%e [ Nothing_uk; A_pair_uk (532, -4433); Nothing2_uk ]] );
              ( "behemoth",
                [%t: behemoth],
                [%e behemoth_rt_id],
                [%e pp_behemoth],
                [%e equal_behemoth],
                [%e
                  [
                    {
                      d = { s = "Coucou" };
                      b = B;
                      p = SomeInt 101;
                      pair = ("hello", { somevar = 1; r = { a = 0; b = 56000 } });
                      pair_alias = ("coin", 8);
                    };
                  ]] );
              ( "collection",
                [%t: collection],
                [%e collection_rt_id],
                [%e pp_collection],
                [%e equal_collection],
                [%e [ [ 12; 953878; 1; 0; 0; 3; -43 ] ]] );
              ( "int64_alias",
                [%t: int64],
                [%e int64_alias_rt_id],
                [%e pp_int64_alias],
                [%e equal_int64_alias],
                [%e [ 0L; -1L; 8000000000L ]] );
              ( "bool_alias",
                [%t: bool],
                [%e bool_alias_rt_id],
                [%e pp_bool_alias],
                [%e equal_bool_alias],
                [%e [ true; false ]] );
              ( "float_alias",
                [%t: float],
                [%e float_alias_rt_id],
                [%e pp_float_alias],
                [%e equal_float_alias],
                [%e [ -1343.; 0.; 1.; 1e65; 1e300 ]] );
              ( "int_option_alias",
                [%t: int_option_alias],
                [%e int_option_alias_rt_id],
                [%e pp_int_option_alias],
                [%e equal_int_option_alias],
                [%e [ None; Some 0; Some (-1); Some 1 ]] );
              ( "string_option_record",
                [%t: string_option_record],
                [%e string_option_record_rt_id],
                [%e pp_string_option_record],
                [%e equal_string_option_record],
                [%e
                  [
                    { maybe = None };
                    { maybe = Some "hello" };
                    { maybe = Some "coin" };
                  ]] );
              ( "float_map_alias",
                [%t: float_map_alias],
                [%e float_map_alias_rt_id],
                [%e pp_float_map_alias],
                [%e equal_float_map_alias],
                [%e [ []; [ ("somekey", 54.); ("testy testo", 0.) ] ]] );
              ( "record_with_indir_field",
                [%t: record_with_indir_field],
                [%e record_with_indir_field_rt_id],
                [%e pp_record_with_indir_field],
                [%e equal_record_with_indir_field],
                [%e [ { indir = { a = -2; b = 43 } } ]] );
              ( "with_computed",
                [%t: with_computed],
                [%e with_computed_rt_id],
                [%e pp_with_computed],
                [%e equal_with_computed],
                [%e [ { non_computed = 4243 } ]] );
              ( "json_alias",
                [%t: json_alias],
                [%e json_alias_rt_id],
                [%e pp_json_alias],
                [%e equal_json_alias],
                [%e
                  [
                    `A
                      [
                        `A [];
                        `Float 65.;
                        `O [ ("mykey", `Bool true); ("otherKey", `Null) ];
                      ];
                  ]] );
              ( "time_alias",
                [%t: time_alias],
                [%e time_alias_rt_id],
                [%e pp_time_alias],
                [%e equal_time_alias],
                [%e [ ptime_sample ]] );
              ( "time_local_alias",
                [%t: time_local_alias],
                [%e time_local_alias_rt_id],
                [%e pp_time_local_alias],
                [%e equal_time_local_alias],
                [%e [ (ptime_sample, 0); (ptime_sample, 5443) ]] );
              ( "date_alias",
                [%t: date_alias],
                [%e date_alias_rt_id],
                [%e Fmt.any ""],
                [%e equal_date_alias],
                [%e [ (2022, 01, 01); (1970, 01, 01) ]] );
              ( "record_with_unit",
                [%t: record_with_unit],
                [%e record_with_unit_rt_id],
                [%e pp_record_with_unit],
                [%e equal_record_with_unit],
                [%e [ { concrete = 24322443; nothing = (); after_unit = 4 } ]]
              );
              ( "string_wrapper_parametric_record",
                [%t: string_wrapper parametric_record],
                [%e string_wrapper_parametric_record_rt_id],
                [%e pp_parametric_record pp_string_wrapper],
                [%e equal_parametric_record equal_string_wrapper],
                [%e [ { parameterized_field = "Helloooooo!" } ]] );
              ( "poly_basic",
                [%t: poly_basic],
                [%e poly_basic_rt_id],
                [%e pp_poly_basic],
                [%e equal_poly_basic],
                [%e [ `A; `B; `C ]] );
              ( "poly_payload",
                [%t: poly_payload],
                [%e poly_payload_rt_id],
                [%e pp_poly_payload],
                [%e equal_poly_payload],
                [%e [ `A; `B; `Single 532442; `Tuple (213, ":)") ]] );
              ( "string_wrapper_parametric_record_ultra_generic",
                [%t: reified_ultra_generic],
                [%e string_wrapper_parametric_record_ultra_generic_rt_id],
                [%e
                  pp_ultra_generic Base.Int.pp
                    (pp_parametric_record pp_string_wrapper)
                    pp_string_wrapper],
                [%e
                  equal_ultra_generic Int.equal
                    (equal_parametric_record equal_string_wrapper)
                    equal_string_wrapper],
                [%e
                  [
                    {
                      (* Fully parameterized field *)
                      field = -432;
                      (* Partially parameterized field, with a builtin *)
                      list = [ { parameterized_field = "cocorico" } ];
                      (* Partially parameterized field, with a user-defined
                         type *)
                      nested_generic = { parameterized_field = "Coin" };
                    };
                  ]] );
              ( "event_1",
                [%t: Event_1.t],
                [%e event_1_rt_id],
                [%e Event_1.pp],
                [%e Event_1.equal],
                [%e [ { wrapped = `B }; { wrapped = `A } ]] );
              ( "mapped",
                [%t: mapped],
                [%e mapped_rt_id],
                [%e pp_mapped],
                [%e equal_mapped],
                [%e ([ B; A ] : mapped list)] );
              ( "reified_remote_1",
                [%t: Reified_remote_1.t],
                [%e reified_remote_1_rt_id],
                [%e Reified_remote_1.pp],
                [%e Reified_remote_1.equal],
                [%e
                  [
                    None;
                    Some Missing;
                    Some (Loaded { value = { a = 53; b = -3 } });
                  ]] );
              ( "reified_remote_2",
                [%t: Reified_remote_2.t],
                [%e reified_remote_2_rt_id],
                [%e Reified_remote_2.pp],
                [%e Reified_remote_2.equal],
                [%e [ None; Some Missing; Some (Loaded { value = 99 }) ]] );
            ]] );
    ]

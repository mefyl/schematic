open Base
open Schematic
open Swift_test_utils

let test ?name ?(deps = []) t =
  let module M = (val t : Schema_test) in
  let name =
    let maybe_id = Schema.unpack { unpack = (fun s -> s.id) } M.schema in
    Option.value name
      ~default:(Option.value maybe_id ~default:"<unnamed schema>")
  in
  let srcs =
    M.swift
    :: List.map deps ~f:(fun m ->
           let module M = (val m : Schema_test) in
           M.swift)
  in
  Alcotest.test_case name `Quick (fun () ->
      let unpacker : _ Schema.unpacker =
        { unpack = (fun schema -> Generate_types.test schema srcs) }
      in
      Schema.unpack unpacker M.schema)

let () =
  let open Alcotest in
  Schematic_ffi.Swift.c_module_name := Some "Schemas";
  run "Schematic.C.Swift"
    Cases.
      [
        ( "typegen",
          [
            test (module Primitive_record);
            test ~deps:[ (module Primitive_record) ] (module Wrapper);
            test (module Basic_variant);
            test (module Payload_variant);
          ] );
      ]

open Base
open Schematic
open C_test_types

let reified_parametric_record_schema =
  parametric_record_schema string_wrapper_schema

let reified_ultra_generic_schema =
  ultra_generic_schema Schemas.int_schema reified_parametric_record_schema
    Schemas.string_schema

let schemas =
  [
    Schema.pack primitive_record_schema;
    Schema.pack collection_schema;
    Schema.pack wrapper_schema;
    Schema.pack float_map_alias_schema;
    Schema.pack behemoth_schema;
    Schema.pack float_alias_schema;
    Schema.pack int_option_alias_schema;
    Schema.pack bool_alias_schema;
    Schema.pack int64_alias_schema;
    Schema.pack record_with_indir_field_schema;
    Schema.pack record_containing_ambiguous_record_schema;
    Schema.pack any_alias_schema;
    Schema.pack time_alias_schema;
    Schema.pack time_local_alias_schema;
    Schema.pack date_alias_schema;
    Schema.pack record_with_unit_schema;
    Schema.pack reified_parametric_record_schema;
    Schema.pack variant_with_inline_record_payload_schema;
    Schema.pack time_schema;
    Schema.pack payload_variant_with_union_key_schema;
    Schema.pack string_option_record_schema;
    (* Schema.pack Weekdays.schema; *)
    Schema.pack proto_rule_schema;
    Schema.pack postpone_schema;
    Schema.pack poly_basic_schema;
    Schema.pack poly_payload_schema;
    Schema.pack evil_defaults_schema;
    Schema.pack reified_ultra_generic_schema;
    Schema.pack tuple_int_string_alias_schema;
    Schema.pack with_computed_schema;
    Schema.pack mapped_schema;
  ]

let () =
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());

  Logs.err (fun m ->
      m "primitive_record schema: %a@." Schematic.Schema.pp
        primitive_record_schema);

  let argv = Sys.get_argv () in
  let h_filename, c_filename = (argv.(1), argv.(2)) in
  let h_contents, c_contents = C.export_schemas ~safety_checks:false schemas in
  let c_contents = Fmt.str "#include \"%s\"" h_filename ^ c_contents in
  let open Stdio.Out_channel in
  write_all h_filename ~data:h_contents;
  write_all c_filename ~data:c_contents

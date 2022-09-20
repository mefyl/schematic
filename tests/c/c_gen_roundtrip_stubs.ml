open Base
open C_test_types

let nl_concat = String.concat ~sep:"\n"

let ctypes =
  let open Schematic in
  C.of_schemas_state C.state_empty
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
    ]
  |> fst |> List.rev

let cgens = ctypes |> List.map ~f:Schematic.C.export

let declarations, impls =
  let typedefs = cgens |> List.map ~f:Schematic.C.typedef_src |> nl_concat in
  let signatures, impls =
    cgens
    |> List.map ~f:(fun Schematic.C.{ to_caml; of_caml; _ } ->
           Schematic.C.
             ( List.map [ to_caml; of_caml ] ~f:signature_src,
               List.map [ to_caml; of_caml ] ~f:impl_src ))
    |> List.unzip
  in
  let signatures = List.concat signatures |> nl_concat in
  let impls = List.concat impls |> nl_concat in
  ([ typedefs; signatures ] |> nl_concat, impls)

let rts = ctypes |> List.map ~f:Schematic.C.roundtrip_src |> nl_concat

let () =
  Fmt.pr "%s\n" Schematic.C.runtime;
  Fmt.pr "\n";
  Fmt.pr "//\n";
  Fmt.pr "// Generated declarations\n";
  Fmt.pr "//\n";
  Fmt.pr "%s" declarations;
  Fmt.pr "\n";
  Fmt.pr "//\n";
  Fmt.pr "// Generated implementations\n";
  Fmt.pr "//\n";
  Fmt.pr "%s" impls;
  Fmt.pr "\n";
  Fmt.pr "\n";
  Fmt.pr "%s" rts;
  ()

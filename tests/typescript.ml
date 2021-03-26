type record = {
  a : int;
  inline : inline; [@schematic.inline]
  c : string;
}

and inline = { b : float option }

and nullable = bool option

and tuple = string * int64 * unit

and variant =
  | Record of record
  | Otherwise of { i : int }

and keyed_variant =
  | KRecord of record
  | KOtherwise of { i : int }
[@@schematic.union_key "kind"] [@@deriving schema]

module Generate_types = struct
  let test t expected =
    Alcotest.(
      check string "typescript declarations" expected
      @@ Schematic.Typescript.(to_string @@ of_schema t))

  let nullable () = test nullable_schema "boolean | null"

  let record () =
    test record_schema {|{ "a": number; "b"?: number; "c": string }|}

  let tuple () = test tuple_schema "[ string, number, null ]"

  let variant () =
    test variant_schema {|Record | { "i": number }|};
    test keyed_variant_schema
      {|Record & { "kind": "krecord" } | { "i": number } & { "kind": "kotherwise" }|}
end

let () =
  let open Alcotest in
  run "Schematic.Typescript"
    [
      ( "generate types",
        [
          test_case "nullable" `Quick Generate_types.nullable;
          test_case "record" `Quick Generate_types.record;
          test_case "tuple" `Quick Generate_types.tuple;
          test_case "variant" `Quick Generate_types.variant;
        ] );
    ]

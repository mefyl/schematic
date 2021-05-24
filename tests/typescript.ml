open Base

type record = {
  a : int;
  inline : inline; [@schematic.inline]
  c : string;
}

and inline = { b : float option }

and nullable = bool option

and tuple = string * int64 * unit [@@deriving schema]

type variant =
  | Record of record
  | Otherwise of { i : int }
[@@deriving schema]

type keyed_variant =
  | KRecord of record
  | KOtherwise of { i : int }
[@@schematic.union_key "kind"]

and rec_a = {
  self_a : rec_a;
  other_a : rec_b;
}

and rec_b = {
  self_b : rec_b;
  other_b : rec_a;
}
[@@deriving schema]

module Generate_types = struct
  let test t expected =
    Alcotest.(
      check (list (pair string string)) "typescript declarations" expected
      @@ List.map ~f:(fun (id, schema) ->
             (id, Schematic.Typescript.to_string schema))
      @@ Schematic.Typescript.of_schema t)

  let nullable () = test nullable_schema [ ("Nullable", "boolean | null") ]

  type string_or_int = (string, int) Either.t [@@deriving schema]

  type ('t, 'error) res =
    | Ok of 't
    | Error of 'error
  [@@schematic.union_key "status"] [@@deriving schema]

  type record_res = (record, string) res [@@deriving schema]

  let parametric () =
    test string_or_int_schema
      [
        ("StringOrInt", "Either<string, number>");
        ("Either<First, Second>", "First | Second");
      ]
  (*; test record_res_schema [ ("RecordRes", "Either<string, number>");
    ("Result<T1, T2>", "T1 | T2") ]*)

  let record () =
    test record_schema
      [ ("Record", {|{ "a": number; "b"?: number; "c": string }|}) ]

  let tuple () = test tuple_schema [ ("Tuple", "[ string, number, null ]") ]

  let variant () =
    let variant = ("Variant", {|Record | { "i": number }|})
    and keyed =
      ( "KeyedVariant",
        {|Record & { "kind": "krecord" } | { "i": number } & { "kind": "kotherwise" }|}
      )
    and record = ("Record", {|{ "a": number; "b"?: number; "c": string }|}) in
    test variant_schema [ variant; record ];
    test keyed_variant_schema [ keyed; record ]

  let recursive () =
    let rec_a = ("RecA", {|{ "self_a": RecA; "other_a": RecB }|})
    and rec_b = ("RecB", {|{ "self_b": RecB; "other_b": RecA }|}) in
    test rec_a_schema [ rec_a; rec_b ];
    test rec_b_schema [ rec_b; rec_a ]
end

let () =
  let open Alcotest in
  run "Schematic.Typescript"
    [
      ( "generate types",
        [
          test_case "nullable" `Quick Generate_types.nullable;
          test_case "parametric" `Quick Generate_types.parametric;
          test_case "record" `Quick Generate_types.record;
          test_case "recursive" `Quick Generate_types.recursive;
          test_case "tuple" `Quick Generate_types.tuple;
          test_case "variant" `Quick Generate_types.variant;
        ] );
    ]

open Base

module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

type record = {
  a : int;
  inline : inline; [@schematic.inline]
  c : string;
}

and inline = { b : float option }

and inline_variant = {
  coin : string;
  pan : variant; [@schematic.inline]
}

and nullable = bool option
and tuple = string * int64 * unit

and variant =
  | Record of record
  | Otherwise of { i : int }
[@@deriving schema]

let tuple_typescript = ("Tuple", "[ string, number, null ]")

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

type inline_keyed_variant = {
  coin : string;
  pan : keyed_variant; [@schematic.inline]
}
[@@deriving schema]

module ShouldInlineOptions = struct
  (* nested inline optional while hiding type with a functor *)

  module Hide = struct
    type 'a t = 'a

    let schema_versioned _ s = s
  end

  type fields = {
    a : string;
    a_opt : string option;
  }
  [@@deriving schema]

  type inner = {
    mandatory : string Hide.t; [@schematic.as "mandatory_id"]
    inside_struct : fields Hide.t; [@schematic.inline]
  }
  [@@deriving schema]

  type t = { inner : inner option Hide.t [@schematic.inline] [@default None] }
  [@@deriving schema]
end

let record_typescript =
  ("Record", {|{ "a": number; "b"?: number; "c": string }|})

let variant_typescript = ("Variant", {|Record | { "i": number }|})

module Generate_types = struct
  let test t expected =
    Alcotest.(
      check (list (pair string string)) "typescript declarations" expected
      @@ List.map ~f:(fun (id, schema) ->
             (id, Schematic.Typescript.to_string schema))
      @@ Schematic.Typescript.of_schema t)

  let nullable () = test nullable_schema [ ("Nullable", "boolean | null") ]
  let record () = test record_schema [ record_typescript ]

  type string_or_int = (string, int) Either.t [@@deriving schema]

  type ('t, 'error) res =
    | Ok of 't
    | Error of 'error
  [@@schematic.union_key "status"]
  [@@schematic.name "result"]
  [@@deriving schema]

  type record_res = (record, string) res [@@deriving schema]

  let parametric () =
    test string_or_int_schema
      [
        ("StringOrInt", "Either<string, number>");
        ("Either<First, Second>", "First | Second");
      ];
    test record_res_schema
      [
        ("RecordRes", "Result<Record, string>");
        ( "Result<T, Error>",
          {|T & { "status": "ok" } | Error & { "status": "error" }|} );
        record_typescript;
      ]

  let tuple () = test tuple_schema [ tuple_typescript ]

  let variant () =
    let keyed =
      ( "KeyedVariant",
        {|Record & { "kind": "krecord" } | { "i": number } & { "kind": "kotherwise" }|}
      )
    and record = record_typescript in
    test variant_schema [ variant_typescript; record ];
    test keyed_variant_schema [ keyed; record ]

  let inline_variant () =
    test inline_variant_schema
      [
        ( "InlineVariant",
          {|{ "coin": string; "a": number; "b"?: number; "c": string } |
{ "coin": string; "i": number }|}
        );
      ]

  let inline_keyed_variant () =
    test inline_keyed_variant_schema
      [
        ( "InlineKeyedVariant",
          {|{ "coin": string; "kind": "krecord"; "a": number; "b"?: number; "c": string } |
{ "coin": string; "kind": "kotherwise"; "i": number }|}
        );
      ]

  type 'a inline_try = {
    maybe : bool;
    inline : 'a; [@schematic.inline_try]
  }
  [@@deriving schema]

  let inline_try () =
    test
      { (inline_try_schema record_schema) with parametric = None }
      [
        ( "RecordInlineTry",
          {|{ "maybe": boolean; "a": number; "b"?: number; "c": string }|} );
      ];
    test
      { (inline_try_schema tuple_schema) with parametric = None }
      [
        ("TupleInlineTry", {|{ "maybe": boolean; "inline": Tuple }|});
        tuple_typescript;
      ]

  let recursive () =
    let rec_a = ("RecA", {|{ "self_a": RecA; "other_a": RecB }|})
    and rec_b = ("RecB", {|{ "self_b": RecB; "other_b": RecA }|}) in
    test rec_a_schema [ rec_a; rec_b ];
    test rec_b_schema [ rec_b; rec_a ]

  let hidden_inline_optional () =
    test ShouldInlineOptions.schema
      [
        ( "Shouldinlineoptions",
          {|{ "mandatory_id"?: string; "a"?: string; "a_opt"?: string }|} );
      ]
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
          test_case "inline-variant" `Quick Generate_types.inline_variant;
          test_case "inline-try" `Quick Generate_types.inline_try;
          test_case "inline-option" `Quick Generate_types.hidden_inline_optional;
          test_case "inline-keyed-variant" `Quick
            Generate_types.inline_keyed_variant;
        ] );
    ]

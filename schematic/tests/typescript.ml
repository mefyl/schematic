open Base

module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

type 'a hidden_option = 'a option [@@deriving schema]

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

and option_option = { oo : int option option }

and option_default = { od : int option hidden_option [@default None] }
[@@deriving schema]

let inline_typescript = ("Inline", {|{ "b"?: number }|})
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

module Computed = struct
  type t = {
    x : int;
    y : int;
  }
  [@@schematic.computed
    {
      sum : int = x + y;
      ratio : int option = (if y <> 0 then Some (x / y) else None);
    }]
  [@@deriving schema]
end

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
  [ inline_typescript; ("Record", {|{ "a": number; "c": string } & Inline|}) ]

let variant_typescript = ("Variant", {|Record | { "i": number }|})

module Generate_types = struct
  open Test_utils

  let test ?(annotate_since = false) ~here t (expected : _ list) =
    let expected = Base.Set.of_list (module String_pair) expected in
    let actual =
      Schematic.Typescript.of_schema t
      |> List.map ~f:(fun (id, schema) ->
             (id, Schematic.Typescript.to_string ~annotate_since schema))
      |> Base.Set.of_list (module String_pair)
    in
    Alcotest.(
      check ~here Testables.Set.String_pair.set "typescript declarations"
        expected actual)

  let nullable () =
    test ~here:[%here] nullable_schema [ ("Nullable", "boolean | null") ]

  let record () = test ~here:[%here] record_schema record_typescript

  module Parametric = struct
    type string_or_int = (string, int) Schematic.Schemas.Either.t
    [@@deriving schema]

    type ('t, 'error) res =
      | Ok of 't
      | Error of 'error
    [@@schematic.union_key "status"]
    [@@schematic.name "result"]
    [@@deriving schema]

    type record_res = (record, string) res [@@deriving schema]

    let simple () =
      test ~here:[%here] string_or_int_schema
        [
          ("StringOrInt", "Either<string, number>");
          ("Either<First, Second>", "First | Second");
        ];
      test ~here:[%here] record_res_schema
      @@ ("RecordRes", "Result<Record, string>")
         :: ( "Result<T, Error>",
              {|T & { "status": "ok" } | Error & { "status": "error" }|} )
         :: record_typescript

    type 'v variant_inline = V of { v : 'v [@schematic.inline] }
    [@@deriving schema]

    type record = { field : int } [@@deriving schema]
    type inline = record variant_inline [@@deriving schema]

    let inline () =
      test ~here:[%here] inline_schema
        [
          ("Inline", "VariantInline<Record>");
          ("Record", "{ \"field\": number }");
          ("VariantInline<V>", "{  } & V");
        ]

    type inline_ignore = Schematic.Schemas.ignore variant_inline
    [@@deriving schema]

    let inline_ignore () =
      test ~here:[%here] inline_ignore_schema
        [
          ("InlineIgnore", "VariantInline<{  }>");
          ("VariantInline<V>", "{  } & V");
        ]
  end

  let tuple () = test ~here:[%here] tuple_schema [ tuple_typescript ]

  let variant () =
    let keyed =
      ( "KeyedVariant",
        {|Record & { "kind": "krecord" } | { "i": number } & { "kind": "kotherwise" }|}
      )
    and record = record_typescript in
    test ~here:[%here] variant_schema (variant_typescript :: record);
    test ~here:[%here] keyed_variant_schema (keyed :: record)

  let inline_variant () =
    test ~here:[%here] inline_variant_schema
      [
        inline_typescript;
        ("InlineVariant", {|{ "coin": string } & Variant|});
        ("Record", "{ \"a\": number; \"c\": string } & Inline");
        ("Variant", "Record | { \"i\": number }");
      ]

  let inline_keyed_variant () =
    test ~here:[%here] inline_keyed_variant_schema
    @@ ("InlineKeyedVariant", {|{ "coin": string } & KeyedVariant|})
       :: ( "KeyedVariant",
            {|Record & { "kind": "krecord" } | { "i": number } & { "kind": "kotherwise" }|}
          )
       :: record_typescript

  type 'a inline_try = {
    maybe : bool;
    inline : 'a; [@schematic.inline_try]
  }
  [@@deriving schema]

  let inline_try () =
    test ~here:[%here]
      { (inline_try_schema record_schema) with parametric = None }
      [
        inline_typescript;
        ( "RecordInlineTry",
          {|{ "maybe": boolean; "a": number; "c": string } & Inline|} );
      ];
    test ~here:[%here]
      { (inline_try_schema tuple_schema) with parametric = None }
      [
        ("TupleInlineTry", {|{ "maybe": boolean; "inline": Tuple }|});
        tuple_typescript;
      ]

  let recursive () =
    let rec_a = ("RecA", {|{ "self_a": RecA; "other_a": RecB }|})
    and rec_b = ("RecB", {|{ "self_b": RecB; "other_b": RecA }|}) in
    test ~here:[%here] rec_a_schema [ rec_a; rec_b ];
    test ~here:[%here] rec_b_schema [ rec_b; rec_a ]

  let hidden_inline_optional () =
    test ~here:[%here] ShouldInlineOptions.schema
      [
        ("Fields", {|{ "a": string; "a_opt"?: string }|});
        ("Shouldinlineoptions", {|{ "mandatory_id"?: string } & Fields|});
      ]

  let computed () =
    test ~here:[%here] Computed.schema
      [
        ( "Computed",
          {|{ "x": number; "y": number; "ratio"?: number; "sum": number }|} );
      ]

  let option_option () =
    test ~here:[%here] option_option_schema
      [ ("OptionOption", {|{ "oo"?: number | null }|}) ]

  let option_default () =
    test ~here:[%here] option_default_schema
      [
        ("IntOptionHiddenOption", {|number | null | null|});
        ("OptionDefault", {|{ "od"?: IntOptionHiddenOption }|});
      ]

  type a = A
  and b = B
  and c = C

  and duplicate = {
    a : a;
    b : b;
    b' : b;
    c : c;
  }
  [@@deriving schema]

  let duplicate () =
    test ~here:[%here] duplicate_schema
      [
        ("A", {|"a"|});
        ("B", {|"b"|});
        ("C", {|"c"|});
        ("Duplicate", {|{ "a": A; "b": B; "b'": B; "c": C }|});
      ]

  type union_inner_variant =
    | A of { a : int }
    | B of { b : int }
  [@@schematic.name_opt None]

  and union_inner = Ctor of union_inner_variant
  [@@schematic.union_key "kind"] [@@deriving schema]

  type union_outer_variant = Ctor of { b : int }
  [@@schematic.union_key "kind"] [@@schematic.name_opt None]

  and union_outer =
    | A of { a : int }
    | B of union_outer_variant
  [@@deriving schema]

  let priorities () =
    test ~here:[%here] union_inner_schema
      [
        ( "UnionInner",
          {|({ "a": number } | { "b": number }) & { "kind": "ctor" }|} );
      ];
    test ~here:[%here] union_outer_schema
      [
        ( "UnionOuter",
          {|{ "a": number } | { "b": number } & { "kind": "ctor" }|} );
      ]

  type versioned_record = {
    x : int;
    y : int;
    z : int option; [@schematic.since "0.2.0"]
  }
  [@@deriving schema]

  let since_annotation_record () =
    test ~annotate_since:true ~here:[%here] versioned_record_schema
      [ ("VersionedRecord", {|{ "x": number; "y": number; /* since 0.2.0 */ "z"?: number }|}) ]

  type versioned_variant =
    | A of { v : int }
    | B of { v : int }
    | C of { v : int } [@schematic.since "0.2.0"]
  [@@deriving schema]

  let since_annotation_variant () =
    test ~annotate_since:true ~here:[%here] versioned_variant_schema
      [ ("VersionedVariant", {|{ "v": number } | { "v": number } | { "v": number } /* since 0.2.0 */|}) ]
end

let () =
  let open Alcotest in
  run "Schematic.Typescript"
    [
      ( "generate types",
        Generate_types.
          [
            test_case "nullable" `Quick nullable;
            test_case "parametric-simple" `Quick Parametric.simple;
            test_case "parametric inline" `Quick Parametric.inline;
            test_case "parametric inline ignore" `Quick Parametric.inline_ignore;
            test_case "record" `Quick record;
            test_case "recursive" `Quick recursive;
            test_case "tuple" `Quick tuple;
            test_case "variant" `Quick variant;
            test_case "inline-variant" `Quick inline_variant;
            test_case "inline-try" `Quick inline_try;
            test_case "inline-option" `Quick hidden_inline_optional;
            test_case "inline-keyed-variant" `Quick inline_keyed_variant;
            test_case "computed" `Quick computed;
            test_case "option option" `Quick option_option;
            test_case "option option" `Quick option_default;
            test_case "duplicate type" `Quick duplicate;
            test_case "priorities" `Quick priorities;
            test_case "since annotation record" `Quick since_annotation_record;
            test_case "since annotation variant" `Quick since_annotation_variant;
          ] );
    ]

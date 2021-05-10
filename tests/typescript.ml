open Base

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
  let test ~recursive t expected =
    Alcotest.(
      check (list string) "typescript declarations" expected
      @@ List.map ~f:Schematic.Typescript.to_string
      @@ Schematic.Typescript.of_schema ~recursive t)

  let nullable () =
    let exp = [ "boolean | null" ] in
    test ~recursive:false nullable_schema exp;
    test ~recursive:true nullable_schema exp

  let record () =
    let exp = [ {|{ "a": number; "b"?: number; "c": string }|} ] in
    test ~recursive:false record_schema exp;
    test ~recursive:true record_schema exp

  let tuple () =
    let exp = [ "[ string, number, null ]" ] in
    test ~recursive:false tuple_schema exp;
    test ~recursive:true tuple_schema exp

  let variant () =
    let variant = {|Record | { "i": number }|}
    and keyed =
      {|Record & { "kind": "krecord" } | { "i": number } & { "kind": "kotherwise" }|}
    and record = {|{ "a": number; "b"?: number; "c": string }|} in
    test ~recursive:false variant_schema [ variant ];
    test ~recursive:true variant_schema [ variant; record ];
    test ~recursive:false keyed_variant_schema [ keyed ];
    test ~recursive:true keyed_variant_schema [ keyed; record ]

  let recursive () =
    let rec_a = {|{ "self_a": RecA; "other_a": RecB }|}
    and rec_b = {|{ "self_b": RecB; "other_b": RecA }|} in
    test ~recursive:false rec_a_schema [ rec_a ];
    test ~recursive:true rec_a_schema [ rec_a; rec_b ];
    test ~recursive:false rec_b_schema [ rec_b ];
    test ~recursive:true rec_b_schema [ rec_b; rec_a ]
end

let () =
  let open Alcotest in
  run "Schematic.Typescript"
    [
      ( "generate types",
        [
          test_case "nullable" `Quick Generate_types.nullable;
          test_case "record" `Quick Generate_types.record;
          test_case "recursive" `Quick Generate_types.recursive;
          test_case "tuple" `Quick Generate_types.tuple;
          test_case "variant" `Quick Generate_types.variant;
        ] );
    ]

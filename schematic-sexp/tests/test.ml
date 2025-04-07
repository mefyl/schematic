module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

let roundtrip test schema value () =
  let tumbled =
    let string = Schematic_sexp.Stream.to_string schema value in
    Fmt.epr "repr: %S@." string;
    Schematic_sexp.Stream.of_string schema string
  in
  Alcotest.(check ~here:[%here] (result test (module Schematic.Error.Decoding)))
    "tumbled value" tumbled (Result.Ok value)

let trailing_garbage () =
  Alcotest.(check ~here:[%here] (result int pass))
    "reading integer" (Result.Error Schematic.Error.Decoding.empty)
  @@ Schematic_sexp.Stream.of_string [%schema: int] "42 ()"

type point = {
  x : int;
  y : int;
}

and circle = {
  center : point;
  diameter : int;
}

and square = {
  top_left : point;
  bottom_right : point;
}

and shape =
  | Circle of circle
  | Square of square
[@@deriving eq, schema, show]

let circle = { center = { x = 52; y = 42 }; diameter = 70 }

type cons = { next : cons option } [@@deriving eq, schema, show]

let () =
  let test_case name = Alcotest.test_case name `Quick in
  Alcotest.run "Schematic_sexp"
    [
      ( "roundtrip",
        [
          test_case "unit" @@ roundtrip Alcotest.unit [%schema: unit] ();
          test_case "int" @@ roundtrip Alcotest.int [%schema: int] 21;
          test_case "float"
          @@ roundtrip (Alcotest.float 0.) [%schema: float] 123.456;
          test_case "string"
          @@ roundtrip Alcotest.string [%schema: string] "LoremIpsum";
          test_case "escaped string"
          @@ roundtrip Alcotest.string [%schema: string] "Choke ) on #; this";
          test_case "empty list"
          @@ roundtrip Alcotest.(list int) [%schema: int list] [];
          test_case "int list"
          @@ roundtrip Alcotest.(list int) [%schema: int list] [ 12; 34; 56 ];
          test_case "tuple"
          @@ roundtrip
               Alcotest.(triple int string unit)
               [%schema: int * string * unit] (12, "34", ());
          test_case "circle"
          @@ roundtrip
               Alcotest.(testable pp_circle equal_circle)
               circle_schema circle;
          test_case "objects"
          @@ roundtrip
               Alcotest.(list @@ testable pp_shape equal_shape)
               [%schema: shape list]
               [
                 Circle circle;
                 Square
                   {
                     top_left = { x = 0; y = 0 };
                     bottom_right = { x = 1024; y = 768 };
                   };
               ];
          test_case "recursive"
          @@ roundtrip
               Alcotest.(testable pp_cons equal_cons)
               cons_schema
               { next = Some { next = Some { next = None } } };
        ] );
      ("serialization data", [ test_case "trailing garbage" trailing_garbage ]);
    ]

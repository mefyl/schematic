open Base

(* Once upon a time, there used to be abstraction and design in the code the
   people of these lands produced. But they were smitten by the god of the flops
   and his evil antechrist the garbage collector, and lost all ability to think,
   ending up fiddling with bits and offsets in buffers like monkeys bashing
   keyboards with wrenches.

   And it was a sad and barren land, devoid of any original thought, but the
   servers did not burst in flame anymore and that seemed to appease the
   gods. *)

module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

let roundtrip ~here test schema value () =
  let tumbled =
    let string = Schematic.Marshal.to_string schema value in
    let () = Fmt.epr "repr: %S@." string in
    Schematic.Marshal.of_string schema string
  in
  Alcotest.(check ~here (result test (module Schematic.Error.Decoding)))
    "tumbled value" (Result.Ok value) tumbled

let trailing_garbage () =
  Alcotest.(check ~here:[%here] (result int pass))
    "reading integer" (Result.Error Schematic.Error.Decoding.empty)
  @@ Schematic.Marshal.of_string [%schema: int]
       "\x00\x00\x00\x00\x00\x00\x00\x00xxx"

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

module Opt = struct
  type t = {
    a : int option;
    b : int option;
    c : int option;
  }
  [@@deriving eq, schema, show]
end

let () =
  let test_case name = Alcotest.test_case name `Quick in
  Alcotest.run "Schematic.Marshal"
    [
      ( "roundtrip",
        [
          test_case "unit"
          @@ roundtrip ~here:[%here] Alcotest.unit [%schema: unit] ();
          test_case "int"
          @@ roundtrip ~here:[%here] Alcotest.int [%schema: int] 21;
          test_case "float"
          @@ roundtrip ~here:[%here] (Alcotest.float 0.) [%schema: float]
               123.456;
          test_case "string"
          @@ roundtrip ~here:[%here] Alcotest.string [%schema: string]
               "LoremIpsum";
          test_case "escaped string"
          @@ roundtrip ~here:[%here] Alcotest.string [%schema: string]
               "Choke ) on #; this";
          test_case "empty list"
          @@ roundtrip ~here:[%here] Alcotest.(list int) [%schema: int list] [];
          test_case "int list"
          @@ roundtrip ~here:[%here]
               Alcotest.(list int)
               [%schema: int list] [ 12; 34; 56 ];
          test_case "options"
          @@ roundtrip ~here:[%here]
               Alcotest.(list @@ option int)
               [%schema: int option list] [ Some 12; None; Some 56 ];
          test_case "tuple"
          @@ roundtrip ~here:[%here]
               Alcotest.(triple int string unit)
               [%schema: int * string * unit] (12, "34", ());
          test_case "circle"
          @@ roundtrip ~here:[%here]
               Alcotest.(testable pp_circle equal_circle)
               circle_schema circle;
          test_case "objects"
          @@ roundtrip ~here:[%here]
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
          @@ roundtrip ~here:[%here]
               Alcotest.(testable pp_cons equal_cons)
               cons_schema
               { next = Some { next = Some { next = None } } };
          test_case "JSON"
          @@ roundtrip ~here:[%here]
               (module Schematic.Json)
               Schematic.Json.schema
          @@ Schematic.Json.encode circle_schema circle;
          test_case "JSONs"
          @@ roundtrip ~here:[%here]
               Alcotest.(list (module Schematic.Json))
               Schematic.(Schemas.list_schema Json.schema)
          @@ [ `A []; `Null; `Null; `Null ];
          test_case "optional fields"
          @@ roundtrip ~here:[%here]
               (module Opt)
               Opt.schema
               { a = Some 0; b = None; c = Some 1 };
        ] );
      ("serialization data", [ test_case "trailing garbage" trailing_garbage ]);
    ]

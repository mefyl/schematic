open Schematic.Schemas

let sexp = Alcotest.testable Sexplib.Sexp.pp Sexplib.Sexp.equal

let decoding_error =
  Alcotest.testable Schematic.Error.pp_decoding Schematic.Error.equal_decoding

module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

module Coding = struct
  let roundtrip schema v exp =
    let value =
      Alcotest.testable
        (Schematic.Pp.of_schema schema)
        (Schematic.Schema.equal schema)
    in
    let encoded = Schematic_sexp.encode schema v in
    let () = Alcotest.(check ~here:[%here] sexp "encoded" exp encoded) in
    Alcotest.(check ~here:[%here] (result value decoding_error))
      "decoded" (Result.Ok v)
    @@ Schematic_sexp.decode schema encoded

  let int () = roundtrip int_schema 20 (Atom "20")

  let list () =
    roundtrip (list_schema int_schema) [ 22; 54; 9922 ]
      (List [ Atom "22"; Atom "54"; Atom "9922" ])

  let null () = roundtrip unit_schema () (List [])

  let option () =
    let schema = option_schema (list_schema int_schema) in
    roundtrip schema None (List []);
    roundtrip schema (Some []) (List [ List [] ]);
    roundtrip schema
      (Some [ 1; 2; 3 ])
      (List [ List [ Atom "1"; Atom "2"; Atom "3" ] ])

  type record = {
    a : int;
    b : string;
  }
  [@@deriving schema]

  let record () =
    roundtrip record_schema
      { a = 49; b = "forty-nine" }
      (List
         [ List [ Atom "a"; Atom "49" ]; List [ Atom "b"; Atom "forty-nine" ] ])

  let string () = roundtrip string_schema "20" (Atom "20")
end

module Any = struct
  type any = { value : Schematic_sexp.t } [@@deriving eq, schema, show]

  let any = Alcotest.testable pp_any equal_any

  let decode_fail () =
    Alcotest.(check ~here:[%here] (result any decoding_error))
      "json error"
      (Result.Error
         {
           path = [ "value" ];
           reason = "expected s-expression, got null";
           schema = None;
         })
      (Schematic.Json.decode any_schema (`O [ ("value", `Null) ]))

  let encode_fail () =
    Alcotest.(check_raises ~here:[%here] "json error")
      (Failure "JSON encoder cannot encode s-expression") (fun () ->
        Schematic.Json.encode any_schema { value = Atom "value" } |> ignore)
end

module Stream = struct
  open Base

  module Schemas_library = struct
    type Schematic.Schema.library += V

    let v = V
  end

  let roundtrip ~here test schema value () =
    let tumbled =
      let string = Schematic_sexp.Stream.to_string schema value in
      Fmt.epr "repr: %S@." string;
      Schematic_sexp.Stream.of_string schema string
    in
    Alcotest.(check ~here (result test (module Schematic.Error.Decoding)))
      "tumbled value" (Result.Ok value) tumbled

  let trailing_garbage () =
    Alcotest.(check ~here:[%here] (result reject pass))
      "reading integer" (Result.Error Schematic.Error.Decoding.empty)
    @@ Schematic_sexp.Stream.of_string [%schema: int] "2:42()"

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

  let cases =
    let test_case name = Alcotest.test_case name `Quick in
    [
      test_case "unit"
      @@ roundtrip ~here:[%here] Alcotest.unit [%schema: unit] ();
      test_case "int" @@ roundtrip ~here:[%here] Alcotest.int [%schema: int] 21;
      test_case "float"
      @@ roundtrip ~here:[%here] (Alcotest.float 0.) [%schema: float] 123.456;
      test_case "string"
      @@ roundtrip ~here:[%here] Alcotest.string [%schema: string] "LoremIpsum";
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
      @@ roundtrip ~here:[%here] (module Schematic.Json) Schematic.Json.schema
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
      test_case "trailing garbage" trailing_garbage;
    ]
end

let () =
  let open Alcotest in
  run "schematic-sexp"
    [
      ("coding", [ test_case "null" `Quick Coding.null ]);
      ("int", [ test_case "int" `Quick Coding.int ]);
      ("list", [ test_case "list" `Quick Coding.list ]);
      ("option", [ test_case "option" `Quick Coding.option ]);
      ("record", [ test_case "record" `Quick Coding.record ]);
      ("string", [ test_case "string" `Quick Coding.string ]);
      ( "any",
        [
          test_case "decode_fail" `Quick Any.decode_fail;
          test_case "encode_fail" `Quick Any.encode_fail;
        ] );
      ("streaming", Stream.cases);
    ]

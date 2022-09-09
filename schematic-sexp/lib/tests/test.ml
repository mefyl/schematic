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
    let () = Alcotest.(check sexp "encoded" exp encoded) in
    Alcotest.(check (result value decoding_error)) "decoded" (Result.Ok v)
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
    ]

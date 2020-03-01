type version =
  { major : int
  ; minor : int
  ; subminor : int
  }

and range = version * version [@@deriving schema]

let v1 = { major = 0; minor = 1; subminor = 2 }

and v2 = { major = 3; minor = 4; subminor = 5 }

and v3 = { major = 6; minor = 7; subminor = 8 }

let pp_version fmt { major; minor; subminor } =
  Format.fprintf fmt "%i.%i.%i" major minor subminor

let pp_range fmt (l, r) =
  Format.fprintf fmt "(%a, %a)" pp_version l pp_version r

type version_alias = version [@@deriving schema]

type expectation =
  | Exact of version
  | Range of range
[@@deriving schema]

let pp_expectation fmt = function
  | Exact v -> pp_version fmt v
  | Range (l, r) -> Format.fprintf fmt "(%a, %a)" pp_version l pp_version r

let json = Alcotest.testable Schematic.Json.pp ( = )

let test_version = Alcotest.testable pp_version ( = )

let test_range = Alcotest.testable pp_range ( = )

let test_expectation = Alcotest.testable pp_expectation ( = )

let roundtrip name test schema value j =
  Alcotest.check json name (Schematic.Json.of_string j)
  @@ Schematic.Json.encode schema value;
  Alcotest.check test name value
  @@ Schematic.Json.decode schema (Schematic.Json.of_string j)

let failure () =
  let open Schematic.Schema in
  try
    roundtrip "int atom" Alcotest.int int_schema 28 "29";
    Alcotest.fail "failure test failed"
  with Failure _ -> ()

let atom_int () =
  let open Schematic.Schema in
  roundtrip "int atom" Alcotest.int int_schema 28 "28"

let atom_string () =
  let open Schematic.Schema in
  roundtrip "int atom" Alcotest.string string_schema "trente trois"
    "\"trente trois\""

let record () =
  roundtrip "record" test_version version_schema
    { major = 1; minor = 2; subminor = 3 }
    "{\"major\": 1, \"minor\": 2, \"subminor\": 3}"

let tuple () =
  roundtrip "tuple" test_range range_schema
    ( { major = 1; minor = 2; subminor = 3 }
    , { major = 4; minor = 5; subminor = 6 } )
    "[{\"major\": 1, \"minor\": 2, \"subminor\": 3}, {\"major\": 1, \"minor\": \
     2, \"subminor\": 3}]"

let variant () =
  roundtrip "variant" test_expectation expectation_schema
    (Exact { major = 1; minor = 2; subminor = 3 })
    "{\"major\": 1, \"minor\": 2, \"subminor\": 3}";
  roundtrip "variant" test_expectation expectation_schema
    (Range
       ( { major = 1; minor = 2; subminor = 3 }
       , { major = 4; minor = 5; subminor = 6 } ))
    "[{\"major\": 1, \"minor\": 2, \"subminor\": 3}, {\"major\": 4, \"minor\": \
     5, \"subminor\": 6}]"

let list_test () =
  roundtrip "list"
    (Alcotest.list test_version)
    (Schematic.Schema.list_schema version_schema)
    [] "[]";
  roundtrip "list"
    (Alcotest.list test_version)
    (Schematic.Schema.list_schema version_schema)
    [ { major = 0; minor = 1; subminor = 2 } ]
    "[{\"major\": 0.000000, \"minor\": 1.000000, \"subminor\": 2.000000}].";
  roundtrip "list"
    (Alcotest.list test_version)
    (Schematic.Schema.list_schema version_schema)
    [ { major = 0; minor = 1; subminor = 2 }
    ; { major = 3; minor = 4; subminor = 5 }
    ; { major = 6; minor = 7; subminor = 8 }
    ]
    "[{\"major\": 0, \"minor\": 1, \"subminor\": 2},{\"major\": 3, \"minor\": \
     4, \"subminor\": 5.00000},{\"major\": 6, \"minor\": 7, \"subminor\": 8}]."

type 'a pair = 'a * 'a [@@deriving schema]

let parametric_type () =
  roundtrip "parametric_type"
    (Alcotest.pair test_version test_version)
    (pair_schema version_schema)
    (v1, v3)
    "[{\"major\": 0, \"minor\": 1, \"subminor\": 2},{\"major\": 6, \"minor\": \
     7, \"subminor\": 8}]."

let () =
  let open Alcotest in
  run "Schematic"
    [ ( "atoms"
      , [ test_case "int" `Quick atom_int
        ; test_case "string" `Quick atom_string
        ] )
    ; ( "composite"
      , [ test_case "record" `Quick record; test_case "tuple" `Quick variant ]
      )
    ; ("list", [ test_case "list" `Quick list_test ])
    ; ("parametric_type", [ test_case "parametric_type" `Quick parametric_type ])
    ; ("failure", [ test_case "failure" `Quick failure ])
    ]

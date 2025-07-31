open Base

module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

type versioned =
  | A of { v : int }
  | B of { v : int }
  | C of { v : int } [@schematic.since "0.2.0"]
[@@schematic.union_key "type"] [@@deriving schema]

type inline = { inline : versioned [@schematic.inline] } [@@deriving schema]

let since () =
  let _a, a_version =
    Schematic.Json.encode_versioned versioned_schema (A { v = 14 })
  and _c, c_version =
    Schematic.Json.encode_versioned versioned_schema (C { v = 15 })
  and _c_inlined, c_inlined_version =
    Schematic.Json.encode_versioned inline_schema { inline = C { v = 15 } }
  in
  let () =
    Alcotest.(check ~here:[%here] (option (module Schematic.Version)))
      "unconstrained encoding" None
      (Map.find a_version Schemas_library.V)
  and () =
    Alcotest.(check ~here:[%here] (option (module Schematic.Version)))
      "encoding constrained to >= 0.2.0"
      (Some (Schematic.Version.make 0 2 0))
      (Map.find c_version Schemas_library.V)
  and () =
    Alcotest.(check ~here:[%here] (option (module Schematic.Version)))
      "encoding constrained to >= 0.2.0"
      (Some (Schematic.Version.make 0 2 0))
      (Map.find c_inlined_version Schemas_library.V)
  in
  ()

let () =
  let open Alcotest in
  let test_case name f = test_case name `Quick f in
  run "schematic versioning" [ ("since", [ test_case "since" since ]) ]

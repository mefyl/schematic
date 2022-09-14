open Base

module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

(** typedef * RHS *)
type test_case = string * string

type primitive_record = {
  a : int;
  b : int;
}
[@@deriving schema]

(* TODO: prefixing, for lack of namespacing *)
let primitive_record_c : test_case =
  ("primitive_record", {|
struct {
  int a;
  int b;
}
|})

module Generate_types = struct
  let test t expected =
    Schematic.C.of_schema t
    |> List.map ~f:(fun (id, schema) ->
           (id, "\n" ^ Schematic.C.to_string schema ^ "\n"))
    |> Alcotest.(
         check (list (pair string string)) "typescript declarations" expected)

  let primitive_record () =
    Fmt.pr "primitive_record_schema:\n%a@." Schematic.Schema.pp_descriptor
      primitive_record_schema.descriptor;

    test primitive_record_schema [ primitive_record_c ]
end

let () =
  let open Alcotest in
  run "Schematic.C"
    [
      ("typegen", [ test_case "record" `Quick Generate_types.primitive_record ]);
    ]

open Base

module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

(* TODO: prefixing, for lack of namespacing *)

(** typedef * RHS *)
type test_case = string * string

type primitive_record = {
  a : int;
  b : int;
}
[@@deriving schema]

let primitive_record_c : test_case =
  ("primitive_record", {|
struct {
  int a;
  int b;
}
|})

type type_alias = int [@@deriving schema]

let type_alias_c = ("type_alias", {|
int
|})

module Generate_types = struct
  let test t expected =
    Schematic.C.of_schema t
    |> List.map ~f:(fun (id, schema) ->
           (id, "\n" ^ Schematic.C.to_string schema ^ "\n"))
    |> Alcotest.(
         check (list (pair string string)) "typescript declarations" expected)

  let primitive_record () = test primitive_record_schema [ primitive_record_c ]
  let type_alias () = test type_alias_schema [ type_alias_c ]
end

let () =
  let open Alcotest in
  run "Schematic.C"
    [
      ( "typegen",
        [
          test_case "type alias" `Quick Generate_types.type_alias;
          test_case "record" `Quick Generate_types.primitive_record;
        ] );
    ]

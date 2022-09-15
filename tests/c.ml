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

type record_voidptr = {
  foo : Ezjsonm.value; [@schematic.schema fun _ -> Schematic.Schemas.any_schema]
}
[@@deriving schema]

(* FIXME: we should probably have a tag. *)
let record_voidptr_c = ("record_voidptr", {|
struct {
  (void)* foo;
}
|})

type wrapper = {
  somevar : int;
  r : primitive_record;
}
[@@deriving schema]

(* XXX: pointers? *)
let wrapper_c =
  ("wrapper", {|
struct {
  int somevar;
  primitive_record r;
}
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
  let record_voidptr () = test record_voidptr_schema [ record_voidptr_c ]

  (* XXX: order? *)
  let nested () = test wrapper_schema [ wrapper_c; primitive_record_c ]
end

let () =
  let open Alcotest in
  run "Schematic.C"
    [
      ( "typegen",
        [
          test_case "type alias" `Quick Generate_types.type_alias;
          test_case "record" `Quick Generate_types.primitive_record;
          test_case "voidptr" `Quick Generate_types.record_voidptr;
          test_case "nested record" `Quick Generate_types.nested;
        ] );
    ]

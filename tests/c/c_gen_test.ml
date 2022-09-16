open Base
open C_test_types

(* TODO: prefixing, for lack of namespacing *)

(** typedef * RHS *)
type test_case = string * string

let primitive_record_c : test_case =
  ("primitive_record", {|
struct {
  int a;
  int b;
}
|})

let primitive_record_of_caml =
  {|
primitive_record* primitive_record_of_caml(value v) {
  CAMLparam1(v);
  primitive_record* p = malloc(sizeof(*p));
  p->a = int_of_caml(Field(v, 0));
  p->b = int_of_caml(Field(v, 1));
  CAMLreturnT(primitive_record*, p);
}
|}

let primitive_record_to_caml =
  {|
value primitive_record_to_caml(primitive_record* p) {
  CAMLparam0();
  CAMLlocal1(v);
  int n_fields = 2;
  int tag = 0;
  v = caml_alloc(n_fields, tag);
  Store_field(v, 0, int_to_caml(p->a));
  Store_field(v, 1, int_to_caml(p->b));
  CAMLreturn(v);
}
|}

let type_alias_c = ("type_alias", {|
int
|})

let wrapper_c =
  ("wrapper", {|
struct {
  int somevar;
  primitive_record* r;
}
|})

let wrapper_of_caml =
  {|
wrapper* wrapper_of_caml(value v) {
  CAMLparam1(v);
  wrapper* p = malloc(sizeof(*p));
  p->somevar = int_of_caml(Field(v, 0));
  p->r = primitive_record_of_caml(Field(v, 1));
  CAMLreturnT(wrapper*, p);
}
|}

let wrapper_to_caml =
  {|
value wrapper_to_caml(wrapper* p) {
  CAMLparam0();
  CAMLlocal1(v);
  int n_fields = 2;
  int tag = 0;
  v = caml_alloc(n_fields, tag);
  Store_field(v, 0, int_to_caml(p->somevar));
  Store_field(v, 1, primitive_record_to_caml(p->r));
  CAMLreturn(v);
}
|}

let string_wrapper_c = ("string_wrapper", {|
char*
|})

(* XXX: get rid of union if there is no payload *)
let basic_variant_c =
  ("basic_variant", {|
enum {
  basic_variant_a,
  basic_variant_b
}
|})

let payload_variant_c =
  ( "payload_variant",
    {|
struct {
  enum {
    payload_variant_nothing_tag,
    payload_variant_someint_tag
  } tag;
  union {
    int someint;
  } payload;
}
|}
  )

let pretty_string =
  let pp_string ppf x = Fmt.pf ppf "{|\n%s\n|}" x in
  Alcotest.testable pp_string String.( = )

module Generate_types = struct
  let test t expected =
    Schematic.C.of_schema t
    |> List.map ~f:(fun (id, schema) ->
           (id, "\n" ^ Schematic.C.to_c_type_expression schema ^ "\n"))
    |> Alcotest.(
         check
           (list (pair pretty_string pretty_string))
           "typescript declarations" expected)

  let primitive_record () = test primitive_record_schema [ primitive_record_c ]
  let type_alias () = test type_alias_schema [ type_alias_c ]
  let nested () = test wrapper_schema [ wrapper_c; primitive_record_c ]
  let string () = test string_wrapper_schema [ string_wrapper_c ]
  let basic_variant () = test basic_variant_schema [ basic_variant_c ]
  let payload_variant () = test payload_variant_schema [ payload_variant_c ]
end

module Of_caml = struct
  let test t expected =
    Schematic.C.of_schema t
    |> List.map ~f:(fun (id, schema) ->
           "\n" ^ Schematic.C.to_of_caml_src id schema ^ "\n")
    |> Alcotest.(check (list pretty_string) "*_of_caml declarations" expected)

  let primitive_record () =
    test primitive_record_schema [ primitive_record_of_caml ]

  let nested () =
    test wrapper_schema [ wrapper_of_caml; primitive_record_of_caml ]
end

module To_caml = struct
  let test t expected =
    Schematic.C.of_schema t
    |> List.map ~f:(fun (id, schema) ->
           "\n" ^ Schematic.C.to_to_caml_src id schema ^ "\n")
    |> Alcotest.(check (list pretty_string) "*_to_caml declarations" expected)

  let primitive_record () =
    test primitive_record_schema [ primitive_record_to_caml ]

  let nested () =
    test wrapper_schema [ wrapper_to_caml; primitive_record_to_caml ]
end

let () =
  let open Alcotest in
  run "Schematic.C"
    [
      ( "typegen",
        [
          test_case "type alias" `Quick Generate_types.type_alias;
          test_case "record" `Quick Generate_types.primitive_record;
          test_case "nested record" `Quick Generate_types.nested;
          test_case "string" `Quick Generate_types.string;
          test_case "basic_variant" `Quick Generate_types.basic_variant;
          test_case "payload_variant" `Quick Generate_types.payload_variant;
        ] );
      ( "of_caml",
        [
          test_case "primitive_record" `Quick Of_caml.primitive_record;
          test_case "nested record" `Quick Of_caml.nested;
        ] );
      ( "to_caml",
        [
          test_case "primitive_record" `Quick To_caml.primitive_record;
          test_case "nested record" `Quick To_caml.nested;
        ] );
    ]

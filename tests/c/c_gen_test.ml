open Base
open C_test_types

(* TODO: prefixing, for lack of namespacing *)

let primitive_record_c = {|
struct {
  int a;
  int b;
}
|}

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

let type_alias_c = {|
int
|}

let wrapper_c = {|
struct {
  int somevar;
  primitive_record* r;
}
|}

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

let string_wrapper_c = {|
char*
|}

(* XXX: get rid of union if there is no payload *)
let basic_variant_c = {|
enum {
  basic_variant_a,
  basic_variant_b
}
|}

let basic_variant_of_caml =
  {|
basic_variant basic_variant_of_caml(value v) {
  basic_variant x = Int_val(v);
  assert(x < 2);
  return x;
}
|}

let basic_variant_to_caml =
  {|
value basic_variant_to_caml(basic_variant x) {
  assert(x < 2);
  return Val_int(x);
}
|}

(* FIXME: possible collisions with "tag" and "payload" *)
let payload_variant_c =
  {|
struct {
  enum {
    payload_variant_nothing_tag,
    payload_variant_nothing2_tag,
    payload_variant_nothing3_tag,
    payload_variant_someint_tag
  } tag;
  union {
    int someint;
  } payload;
}
|}

let payload_variant_of_caml =
  {|
payload_variant payload_variant_of_caml(value v) {
  CAMLparam1(v);
  payload_variant res;
  if (Is_long(v)) {
    res.tag = Int_val(v);
    assert(res.tag < 3);
  } else {
    res.tag = 3 + Tag_val(v);
    assert(Tag_val(v) < 1);
    switch (Tag_val(v)) {
      case 0:
        res.payload.someint = int_of_caml(Field(v, 0));
        break;
    }
  }
  CAMLreturnT(payload_variant, res);
}
|}

let payload_variant_to_caml =
  {|
value payload_variant_to_caml(payload_variant x) {
  CAMLparam0();
  CAMLlocal1(v);
  switch (x.tag) {
    case payload_variant_nothing_tag:
      v = Val_int(0);
      break;
    case payload_variant_nothing2_tag:
      v = Val_int(1);
      break;
    case payload_variant_nothing3_tag:
      v = Val_int(2);
      break;
    case payload_variant_someint_tag:
      v = caml_alloc(1, 0);
      Store_field(v, 0, int_to_caml(x.payload.someint));
      break;
  }
  CAMLreturn(v);
}
|}

let pair_alias_c = {|
tuple_char_ptr_int
|}

let string_int_tuple_c = {|
struct {
  char* t0;
  int t1;
}
|}

(* XXX tuple naming *)
let pair_alias_of_caml =
  {|
pair_alias pair_alias_of_caml(value v) {
  CAMLparam1(v);
  tuple_char_ptr_int res = tuple_char_ptr_int_of_caml(v);
  CAMLreturnT(pair_alias, res);
}
|}

let pair_alias_to_caml =
  {|
value pair_alias_to_caml(pair_alias x) {
  CAMLparam0();
  CAMLlocal1(v);
  v = tuple_char_ptr_int_to_caml(x);
  CAMLreturn(v);
}
|}

let tuple_string_int_of_caml =
  {|
tuple_char_ptr_int tuple_char_ptr_int_of_caml(value v) {
  CAMLparam1(v);
  tuple_char_ptr_int res;
  res.t0 = string_of_caml(Field(v, 0));
  res.t1 = int_of_caml(Field(v, 1));
  CAMLreturnT(tuple_char_ptr_int, res);
}
|}

let tuple_string_int_to_caml =
  {|
value tuple_char_ptr_int_to_caml(tuple_char_ptr_int x) {
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_alloc_tuple(2);
  Store_field(v, 0, string_to_caml(x.t0));
  Store_field(v, 1, int_to_caml(x.t1));
  CAMLreturn(v);
}
|}

let record_with_variant_c = {|
struct {
  payload_variant* v;
}
|}

let record_with_pair_alias_c =
  {|
struct {
  int a;
  pair_alias* pair_alias;
}
|}

let record_with_pair_alias_to_caml = {|
|}

let record_with_pair_alias_of_caml = {|
|}

let pretty_string =
  let pp_string ppf x = Fmt.pf ppf "{|\n%s\n|}" x in
  Alcotest.testable pp_string String.( = )

module Generate_types = struct
  let test t expected =
    Schematic.C.of_schema t
    |> List.map ~f:(fun (_id, schema) ->
           "\n" ^ Schematic.C.to_c_type_expression schema ^ "\n")
    |> Alcotest.(check (list pretty_string) "typescript declarations" expected)

  let primitive_record () = test primitive_record_schema [ primitive_record_c ]
  let type_alias () = test type_alias_schema [ type_alias_c ]
  let nested () = test wrapper_schema [ wrapper_c; primitive_record_c ]
  let string () = test string_wrapper_schema [ string_wrapper_c ]
  let basic_variant () = test basic_variant_schema [ basic_variant_c ]
  let payload_variant () = test payload_variant_schema [ payload_variant_c ]

  let pair_alias () =
    test pair_alias_schema [ pair_alias_c; string_int_tuple_c ]

  let record_with_variant () =
    test record_with_variant_schema [ record_with_variant_c; payload_variant_c ]
end

module Of_caml = struct
  let test t expected =
    Schematic.C.of_schema t
    |> List.map ~f:(fun (id, schema) ->
           Schematic.C.to_of_caml_src id schema |> fun s -> "\n" ^ s ^ "\n")
    |> Alcotest.(check (list pretty_string) "*_of_caml declarations" expected)

  let primitive_record () =
    test primitive_record_schema [ primitive_record_of_caml ]

  let nested () =
    test wrapper_schema [ wrapper_of_caml; primitive_record_of_caml ]

  let basic_variant () = test basic_variant_schema [ basic_variant_of_caml ]

  let payload_variant () =
    test payload_variant_schema [ payload_variant_of_caml ]

  let pair_alias () =
    test pair_alias_schema [ pair_alias_of_caml; tuple_string_int_of_caml ]
end

module To_caml = struct
  let test t expected =
    Schematic.C.of_schema t
    |> List.map ~f:(fun (id, schema) ->
           Schematic.C.to_to_caml_src id schema |> fun s -> "\n" ^ s ^ "\n")
    |> Alcotest.(check (list pretty_string) "*_to_caml declarations" expected)

  let primitive_record () =
    test primitive_record_schema [ primitive_record_to_caml ]

  let nested () =
    test wrapper_schema [ wrapper_to_caml; primitive_record_to_caml ]

  let basic_variant () = test basic_variant_schema [ basic_variant_to_caml ]

  let payload_variant () =
    test payload_variant_schema [ payload_variant_to_caml ]

  let pair_alias () =
    test pair_alias_schema [ pair_alias_to_caml; tuple_string_int_to_caml ]

  let record_with_pair_alias () =
    test record_with_pair_alias_schema [ record_with_pair_alias_to_caml ]
end

let () =
  let open Alcotest in
  run "Schematic.C"
    [
      ( "typegen",
        Generate_types.
          [
            test_case "type alias" `Quick type_alias;
            test_case "record" `Quick primitive_record;
            test_case "nested record" `Quick nested;
            test_case "string" `Quick string;
            test_case "basic_variant" `Quick basic_variant;
            test_case "payload_variant" `Quick payload_variant;
            test_case "pair_alias" `Quick pair_alias;
            test_case "record_with_variant" `Quick record_with_variant;
          ] );
      ( "of_caml",
        Of_caml.
          [
            test_case "primitive_record" `Quick primitive_record;
            test_case "nested record" `Quick nested;
            test_case "basic_variant" `Quick basic_variant;
            test_case "payload_variant" `Quick payload_variant;
            test_case "pair_alias" `Quick pair_alias;
          ] );
      ( "to_caml",
        To_caml.
          [
            test_case "primitive_record" `Quick primitive_record;
            test_case "nested record" `Quick nested;
            test_case "basic_variant" `Quick basic_variant;
            test_case "payload_variant" `Quick payload_variant;
            test_case "pair_alias" `Quick pair_alias
            (* test_case "record_with_pair_alias" `Quick
               record_with_pair_alias; *);
          ] );
    ]

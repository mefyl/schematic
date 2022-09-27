open Base
open C_test_types

(* TODO: prefixing, for lack of namespacing *)

let primitive_record_c =
  {|
typedef struct {
  int a;
  int b;
} primitive_record;
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
typedef int type_alias;
|}

let wrapper_c =
  {|
typedef struct {
  int somevar;
  primitive_record* r;
} wrapper;
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
typedef const char* string_wrapper;
|}

let basic_variant_c =
  {|
typedef enum {
  basic_variant_a,
  basic_variant_b
} basic_variant;
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

let tuple_int_string_c =
  {|
typedef struct {
  int t0;
  const char* t1;
} tuple_int_string;
|}

(* FIXME: possible collisions with "tag" and "payload" *)
let payload_variant_c =
  {|
typedef struct {
  enum {
    payload_variant_nothing_tag,
    payload_variant_nothing2_tag,
    payload_variant_nothing3_tag,
    payload_variant_someint_tag,
    payload_variant_a_pair_tag
  } tag;
  union {
    int someint;
    tuple_int_string a_pair;
  }* payload;
} payload_variant;
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
    res.payload = malloc(sizeof(*res.payload));
    assert(Tag_val(v) < 2);
    switch (Tag_val(v)) {
      case 0:
        res.payload->someint = int_of_caml(Field(v, 0));
        break;
      case 1:
        res.payload->a_pair = tuple_int_string_of_caml(v);
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
      Store_field(v, 0, int_to_caml(x.payload->someint));
      break;
    case payload_variant_a_pair_tag:
      v = caml_alloc(2, 1);
      Store_field(v, 0, int_to_caml(x.payload->a_pair.t0));
      Store_field(v, 1, string_to_caml(x.payload->a_pair.t1));
      break;
  }
  CAMLreturn(v);
}
|}

let pair_alias_c = {|
typedef tuple_string_int pair_alias;
|}

let string_int_tuple_c =
  {|
typedef struct {
  const char* t0;
  int t1;
} tuple_string_int;
|}

(* XXX tuple naming *)
let pair_alias_of_caml =
  {|
pair_alias pair_alias_of_caml(value v) {
  CAMLparam1(v);
  tuple_string_int res = tuple_string_int_of_caml(v);
  CAMLreturnT(pair_alias, res);
}
|}

let pair_alias_to_caml =
  {|
value pair_alias_to_caml(pair_alias x) {
  CAMLparam0();
  CAMLlocal1(v);
  v = tuple_string_int_to_caml(x);
  CAMLreturn(v);
}
|}

let tuple_string_int_of_caml =
  {|
tuple_string_int tuple_string_int_of_caml(value v) {
  CAMLparam1(v);
  tuple_string_int res;
  res.t0 = string_of_caml(Field(v, 0));
  res.t1 = int_of_caml(Field(v, 1));
  CAMLreturnT(tuple_string_int, res);
}
|}

let tuple_string_int_to_caml =
  {|
value tuple_string_int_to_caml(tuple_string_int x) {
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_alloc_tuple(2);
  Store_field(v, 0, string_to_caml(x.t0));
  Store_field(v, 1, int_to_caml(x.t1));
  CAMLreturn(v);
}
|}

let record_with_variant_c =
  {|
typedef struct {
  payload_variant v;
} record_with_variant;
|}

let record_with_pair_alias_c =
  {|
typedef struct {
  int a;
  pair_alias pair_alias;
} record_with_pair_alias;
|}

let record_with_pair_alias_of_caml =
  {|
record_with_pair_alias* record_with_pair_alias_of_caml(value v) {
  CAMLparam1(v);
  record_with_pair_alias* p = malloc(sizeof(*p));
  p->a = int_of_caml(Field(v, 0));
  p->pair_alias = pair_alias_of_caml(Field(v, 1));
  CAMLreturnT(record_with_pair_alias*, p);
}
|}

let record_with_pair_alias_to_caml =
  {|
value record_with_pair_alias_to_caml(record_with_pair_alias* p) {
  CAMLparam0();
  CAMLlocal1(v);
  int n_fields = 2;
  int tag = 0;
  v = caml_alloc(n_fields, tag);
  Store_field(v, 0, int_to_caml(p->a));
  Store_field(v, 1, pair_alias_to_caml(p->pair_alias));
  CAMLreturn(v);
}
|}

let behemoth_c =
  {|
typedef struct {
  diverse* d;
  basic_variant b;
  payload_variant p;
  tuple_string_wrapper pair;
  pair_alias pair_alias;
} behemoth;
|}

let string_wrapper_tuple_c =
  {|
typedef struct {
  const char* t0;
  wrapper* t1;
} tuple_string_wrapper;
|}

let diverse_c = {|
typedef struct {
  const char* s;
} diverse;
|}

let int_array_c = {|
typedef struct {
  size_t len;
  int* data;
} int_array;
|}

let collection_c = {|
typedef int_array collection;
|}

let collection_of_caml =
  {|
collection collection_of_caml(value v) {
  CAMLparam1(v);
  int_array res = int_array_of_caml(v);
  CAMLreturnT(collection, res);
}
|}

let collection_to_caml =
  {|
value collection_to_caml(collection x) {
  CAMLparam0();
  CAMLlocal1(v);
  v = int_array_to_caml(x);
  CAMLreturn(v);
}
|}

let int_array_of_caml =
  {|
int_array int_array_of_caml(value v) {
  CAMLparam1(v);
  CAMLlocal1(l);
  int_array arr = {.len = 0, .data = NULL};
  l = v;
  while (l != Val_emptylist) {
    l = Field(l, 1);
    arr.len++;
  }
  l = v;
  arr.data = malloc(sizeof(*arr.data) * arr.len);
  for (int64_t i = 0; i < arr.len; i++) {
    arr.data[i] = int_of_caml(Field(l, 0));
    l = Field(l, 1);
  }
  CAMLreturnT(int_array, arr);
}
|}

let int_array_to_caml =
  {|
value int_array_to_caml(int_array arr) {
  CAMLparam0();
  CAMLlocal2(cli, cons);
  cli = Val_emptylist;
  for (int64_t i = arr.len - 1; i >= 0; i--) {
    cons = caml_alloc(2, 0);
    typeof(*arr.data) x = arr.data[i];
    Store_field(cons, 0, int_to_caml(x));
    Store_field(cons, 1, cli);
    cli = cons;
  }
  CAMLreturn(cli);
}
|}

let tuple_int_string_of_caml =
  {|
tuple_int_string tuple_int_string_of_caml(value v) {
  CAMLparam1(v);
  tuple_int_string res;
  res.t0 = int_of_caml(Field(v, 0));
  res.t1 = string_of_caml(Field(v, 1));
  CAMLreturnT(tuple_int_string, res);
}
|}

let poly_basic_of_caml =
  {|
poly_basic poly_basic_of_caml(value v) {
  CAMLparam1(v);
  poly_basic x;
  if (0) {}
  else if (v == Val_long(65)) x = poly_basic_a;
  else if (v == Val_long(66)) x = poly_basic_b;
  else if (v == Val_long(67)) x = poly_basic_c;
  else assert(0);
  CAMLreturnT(poly_basic, x);
}
|}

let poly_basic_to_caml =
  {|
value poly_basic_to_caml(poly_basic x) {
  CAMLparam0();
  CAMLlocal1(v);
  switch (x) {
    case poly_basic_a:
      v = Val_long(65);
      break;
    case poly_basic_b:
      v = Val_long(66);
      break;
    case poly_basic_c:
      v = Val_long(67);
      break;
  }
  CAMLreturn(v);
}
|}

let poly_payload_to_caml =
  {|
value poly_payload_to_caml(poly_payload x) {
  CAMLparam0();
  CAMLlocal1(v);
  switch (x.tag) {
    case poly_payload_a_tag:
      v = Val_long(65);
      break;
    case poly_payload_b_tag:
      v = Val_long(66);
      break;
    case poly_payload_single_tag:
      v = caml_alloc_tuple(2);
      Store_field(v, 0, Val_long(-306849112));
      Store_field(v, 1, int_to_caml(x.payload->single));
      break;
    case poly_payload_tuple_tag:
      v = caml_alloc_tuple(2);
      Store_field(v, 0, Val_long(726928360));
      Store_field(v, 1, tuple_int_string_to_caml(x.payload->tuple));
      break;
  }
  CAMLreturn(v);
}
|}

let poly_payload_of_caml =
  {|
poly_payload poly_payload_of_caml(value v) {
  CAMLparam1(v);
  poly_payload x;
  if (0) {}
  else if (v == Val_long(65)) x.tag = poly_payload_a_tag;
  else if (v == Val_long(66)) x.tag = poly_payload_b_tag;
  else if (Is_block(v)) {
    int tag = Field(v, 0);
    x.payload = malloc(sizeof(*x.payload));
    if (0) {}
    else if (tag == Val_long(-306849112)) {
      x.tag = poly_payload_single_tag;
      x.payload->single = int_of_caml(Field(v, 1));
    }
    else if (tag == Val_long(726928360)) {
      x.tag = poly_payload_tuple_tag;
      x.payload->tuple = tuple_int_string_of_caml(Field(v, 1));
    }
    else assert(0);
  }
  else assert(0);
  CAMLreturnT(poly_payload, x);
}
|}

let tuple_int_string_to_caml =
  {|
value tuple_int_string_to_caml(tuple_int_string x) {
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_alloc_tuple(2);
  Store_field(v, 0, int_to_caml(x.t0));
  Store_field(v, 1, string_to_caml(x.t1));
  CAMLreturn(v);
}
|}

let primitive_record_in_parametric_c =
  {|
typedef struct {
  primitive_record* parameterized_field;
} primitive_record_parametric_record;
|}

let string_option_record_c =
  {|
typedef struct {
  string_wrapper_option maybe;
} string_option_record;
|}

let string_wrapper_option_c =
  {|
typedef struct {
  enum {
    string_wrapper_option_none_tag,
    string_wrapper_option_some_tag
  } tag;
  union {
    string_wrapper some;
  }* payload;
} string_wrapper_option;
|}

let pretty_string =
  let pp_string ppf x = Fmt.pf ppf "{|\n%s\n|}" x in
  Alcotest.testable pp_string String.( = )

let check_unordered_strings name expected actual =
  let actual = actual |> List.sort ~compare:String.compare in
  let expected = expected |> List.sort ~compare:String.compare in
  Alcotest.(check (list pretty_string) name expected actual)

module Generate_types = struct
  let test schema expected =
    let actual =
      Schematic.C.of_schema schema
      |> List.map ~f:Schematic.C.export
      |> List.map ~f:(fun gen -> "\n" ^ Schematic.C.typedef_src gen)
      |> List.sort ~compare:String.compare
    in
    check_unordered_strings "typedefs" expected actual

  let primitive_record () = test primitive_record_schema [ primitive_record_c ]
  let type_alias () = test type_alias_schema [ type_alias_c ]
  let nested () = test wrapper_schema [ primitive_record_c; wrapper_c ]
  let string () = test string_wrapper_schema [ string_wrapper_c ]
  let basic_variant () = test basic_variant_schema [ basic_variant_c ]

  let payload_variant () =
    test payload_variant_schema [ tuple_int_string_c; payload_variant_c ]

  let record_with_variant () =
    test record_with_variant_schema
      [ tuple_int_string_c; payload_variant_c; record_with_variant_c ]

  let pair_alias () =
    test pair_alias_schema [ string_int_tuple_c; pair_alias_c ]

  let record_with_pair_alias () =
    test record_with_pair_alias_schema
      [ string_int_tuple_c; pair_alias_c; record_with_pair_alias_c ]

  let behemoth () =
    test behemoth_schema
      [
        primitive_record_c;
        wrapper_c;
        string_wrapper_tuple_c;
        tuple_int_string_c;
        payload_variant_c;
        string_int_tuple_c;
        pair_alias_c;
        diverse_c;
        basic_variant_c;
        behemoth_c;
      ]

  let collection () = test collection_schema [ int_array_c; collection_c ]

  let reified_param () =
    test
      (parametric_record_schema primitive_record_schema)
      [ primitive_record_in_parametric_c; primitive_record_c ]

  let string_option_record () =
    test string_option_record_schema
      [ string_option_record_c; string_wrapper_option_c; string_wrapper_c ]
end

module Of_caml = struct
  let test schema expected =
    let actual =
      Schematic.C.of_schema schema
      |> List.map ~f:Schematic.C.export
      |> List.map ~f:(fun Schematic.C.{ of_caml; _ } ->
             Schematic.C.impl_src of_caml |> fun s -> "\n" ^ s ^ "\n")
    in
    check_unordered_strings "*_of_caml" expected actual

  let primitive_record () =
    test primitive_record_schema [ primitive_record_of_caml ]

  let nested () =
    test wrapper_schema [ wrapper_of_caml; primitive_record_of_caml ]

  let basic_variant () = test basic_variant_schema [ basic_variant_of_caml ]

  let payload_variant () =
    test payload_variant_schema
      [ payload_variant_of_caml; tuple_int_string_of_caml ]

  let pair_alias () =
    test pair_alias_schema [ pair_alias_of_caml; tuple_string_int_of_caml ]

  let record_with_pair_alias () =
    test record_with_pair_alias_schema
      [
        record_with_pair_alias_of_caml;
        pair_alias_of_caml;
        tuple_string_int_of_caml;
      ]

  let collection () =
    test collection_schema [ collection_of_caml; int_array_of_caml ]

  let poly_basic () = test poly_basic_schema [ poly_basic_of_caml ]

  let poly_payload () =
    test poly_payload_schema [ poly_payload_of_caml; tuple_int_string_of_caml ]
end

module To_caml = struct
  let test schema expected =
    let actual =
      Schematic.C.of_schema schema
      |> List.map ~f:Schematic.C.export
      |> List.map ~f:(fun Schematic.C.{ to_caml; _ } ->
             Schematic.C.impl_src to_caml |> fun s -> "\n" ^ s ^ "\n")
    in
    check_unordered_strings "*_to_caml" expected actual

  let primitive_record () =
    test primitive_record_schema [ primitive_record_to_caml ]

  let nested () =
    test wrapper_schema [ wrapper_to_caml; primitive_record_to_caml ]

  let basic_variant () = test basic_variant_schema [ basic_variant_to_caml ]

  let payload_variant () =
    test payload_variant_schema
      [ payload_variant_to_caml; tuple_int_string_to_caml ]

  let pair_alias () =
    test pair_alias_schema [ pair_alias_to_caml; tuple_string_int_to_caml ]

  let record_with_pair_alias () =
    test record_with_pair_alias_schema
      [
        record_with_pair_alias_to_caml;
        pair_alias_to_caml;
        tuple_string_int_to_caml;
      ]

  let collection () =
    test collection_schema [ collection_to_caml; int_array_to_caml ]

  let poly_basic () = test poly_basic_schema [ poly_basic_to_caml ]

  let poly_payload () =
    test poly_payload_schema [ poly_payload_to_caml; tuple_int_string_to_caml ]
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
            test_case "record_with_variant" `Quick record_with_variant;
            test_case "pair_alias" `Quick pair_alias;
            test_case "record_with_pair_alias" `Quick record_with_pair_alias;
            test_case "behemoth" `Quick behemoth;
            test_case "collection" `Quick collection;
            test_case "reified parametric type" `Quick reified_param;
            test_case "string_option_record" `Quick string_option_record;
          ] );
      ( "of_caml",
        Of_caml.
          [
            test_case "primitive_record" `Quick primitive_record;
            test_case "nested record" `Quick nested;
            test_case "basic_variant" `Quick basic_variant;
            test_case "payload_variant" `Quick payload_variant;
            test_case "pair_alias" `Quick pair_alias;
            test_case "record_with_pair_alias" `Quick record_with_pair_alias;
            test_case "collection" `Quick collection;
            test_case "basic polymorphic_variant" `Quick poly_basic;
            test_case "payload polymorphic_variant" `Quick poly_payload;
          ] );
      ( "to_caml",
        To_caml.
          [
            test_case "primitive_record" `Quick primitive_record;
            test_case "nested record" `Quick nested;
            test_case "basic_variant" `Quick basic_variant;
            test_case "payload_variant" `Quick payload_variant;
            test_case "pair_alias" `Quick pair_alias;
            test_case "record_with_pair_alias" `Quick record_with_pair_alias;
            test_case "collection" `Quick collection;
            test_case "basic polymorphic_variant" `Quick poly_basic;
            test_case "payload polymorphic_variant" `Quick poly_payload;
          ] );
    ]

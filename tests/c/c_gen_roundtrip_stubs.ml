open Base
open C_test_types

let headers =
  {|#define CAML_NAME_SPACE
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
|}

let builtins =
  {|
// No GC involved here.
value int_to_caml(int x) {
  return Val_int(x);
}

int int_of_caml(value x) {
  return Int_val(x);
}

value string_to_caml(char* p) {
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_copy_string(p);
  CAMLreturn(v);
}

char* string_of_caml(value v) {
  CAMLparam1(v);
  char* p = strdup(String_val(v));
  CAMLreturnT(char*, p);
}
|}

(* XXX : dedup & smarter order... *)
let ctypes =
  let open Schematic in
  C.of_schemas_state C.state_empty
    ([
       Schema.pack wrapper_schema;
       Schema.pack diverse_schema;
       Schema.pack basic_variant_schema;
       Schema.pack payload_variant_schema;
       Schema.pack pair_alias_schema;
       Schema.pack record_with_pair_alias_schema;
       Schema.pack record_with_variant_schema (* Schema.pack behemoth_schema; *);
     ]
    |> List.rev)
  |> fst |> List.rev

let typedefs =
  ctypes
  |> List.map ~f:(fun (id, schema) ->
         (id, Schematic.C.to_c_type_expression schema))

let typedefs =
  typedefs
  |> List.map ~f:(fun (name, typeexpr) ->
         Fmt.str "typedef %s %s;\n" typeexpr name)
  |> String.concat ~sep:"\n"

let of_caml_srcs =
  ctypes
  |> List.map ~f:(fun (id, schema) -> Schematic.C.to_of_caml_src id schema)
  |> String.concat ~sep:"\n"

let to_caml_srcs =
  ctypes
  |> List.map ~f:(fun (id, schema) -> Schematic.C.to_to_caml_src id schema)
  |> String.concat ~sep:"\n"

(* TODO:

   check that the compiler gives warning when passing a type that contains an
   unboxable type *)

(* TODO: generate roundtrips (must take pointers into account) *)
let rts =
  {|

CAMLprim value sch_wrapper_rt_id(value v) {
  CAMLparam1(v);
  CAMLlocal1(ret);
  wrapper* p = wrapper_of_caml(v);
  ret = wrapper_to_caml(p);
  CAMLreturn(ret);
}


CAMLprim value sch_diverse_rt_id(value v) {
  CAMLparam1(v);
  CAMLlocal1(ret);
  diverse* p = diverse_of_caml(v);
  ret = diverse_to_caml(p);
  CAMLreturn(ret);
}


CAMLprim value sch_basic_variant_rt_id(value v) {
  CAMLparam1(v);
  CAMLlocal1(ret);
  basic_variant p = basic_variant_of_caml(v);
  ret = basic_variant_to_caml(p);
  CAMLreturn(ret);
}


CAMLprim value sch_payload_variant_rt_id(value v) {
  CAMLparam1(v);
  CAMLlocal1(ret);
  payload_variant p = payload_variant_of_caml(v);
  ret = payload_variant_to_caml(p);
  CAMLreturn(ret);
}


// CAMLprim value sch_behemoth_rt_id(value v) {
//   CAMLparam1(v);
//   CAMLlocal1(ret);
//   behemoth* p = behemoth_of_caml(v);
//   ret = behemoth_to_caml(p);
//   CAMLreturn(ret);
// }
|}

let () =
  Fmt.pr "//\n";
  Fmt.pr "// Headers\n";
  Fmt.pr "//\n";
  Fmt.pr "%s" headers;
  Fmt.pr "\n";
  Fmt.pr "\n";
  Fmt.pr "//\n";
  Fmt.pr "// Built-ins\n";
  Fmt.pr "//\n";
  Fmt.pr "%s" builtins;
  Fmt.pr "\n";
  Fmt.pr "\n";
  Fmt.pr "//\n";
  Fmt.pr "// Typedefs\n";
  Fmt.pr "//\n";
  Fmt.pr "%s" typedefs;
  Fmt.pr "\n";
  Fmt.pr "//\n";
  Fmt.pr "// `*_to_caml` functions\n";
  Fmt.pr "//\n";
  Fmt.pr "%s" to_caml_srcs;
  Fmt.pr "\n";
  Fmt.pr "//\n";
  Fmt.pr "// `*_of_caml` functions\n";
  Fmt.pr "//\n";
  Fmt.pr "%s" of_caml_srcs;
  Fmt.pr "\n";
  Fmt.pr "%s" rts;
  ()

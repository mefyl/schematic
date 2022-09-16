open Base
open C_test_types

let headers =
  {|#define CAML_NAME_SPACE
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
|}

let ctypes = Schematic.C.of_schema wrapper_schema |> List.rev

let typedefs =
  ctypes
  |> List.map ~f:(fun (id, schema) ->
         (id, Schematic.C.to_c_type_expression schema))

let typedefs =
  typedefs
  |> List.map ~f:(fun (name, typeexpr) ->
         Fmt.str "typedef %s %s;\n" typeexpr name)
  |> String.concat ~sep:"\n"

let to_caml_srcs =
  ctypes
  |> List.map ~f:(fun (id, schema) -> Schematic.C.to_to_caml_src id schema)
  |> String.concat ~sep:"\n"

let of_caml_srcs =
  ctypes
  |> List.map ~f:(fun (id, schema) -> Schematic.C.to_of_caml_src id schema)
  |> String.concat ~sep:"\n"

let rts =
  {|

CAMLprim value sch_wrapper_rt_id(value v) {
  CAMLparam1(v);
  CAMLlocal1(ret);
  wrapper* p = wrapper_of_caml(v);
  ret = wrapper_to_caml(p);
  CAMLreturn(ret);
}

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

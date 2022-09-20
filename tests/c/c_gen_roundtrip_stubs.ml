open Base
open C_test_types

let headers =
  {|#define CAML_NAME_SPACE
#include <stddef.h>
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
  CAMLparam0();
  CAMLreturn(Val_int(x));
}

int int_of_caml(value x) {
  CAMLparam1(x);
  CAMLreturnT(int, Int_val(x));
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

let nl_concat = String.concat ~sep:"\n"

(* XXX : dedup & smarter order... *)

let ctypes =
  let open Schematic in
  C.of_schemas_state C.state_empty
    [
      (* Schema.pack wrapper_schema; Schema.pack diverse_schema; Schema.pack
         basic_variant_schema; Schema.pack payload_variant_schema; Schema.pack
         pair_alias_schema; Schema.pack record_with_pair_alias_schema;
         Schema.pack record_with_variant_schema; *)
      Schema.pack behemoth_schema;
      Schema.pack collection_schema;
    ]
  |> fst |> List.rev

let cgens = ctypes |> List.map ~f:Schematic.C.export

let declarations, impls =
  let typedefs = cgens |> List.map ~f:Schematic.C.typedef_src |> nl_concat in
  let signatures, impls =
    cgens
    |> List.map ~f:(fun Schematic.C.{ to_caml; of_caml; _ } ->
           Schematic.C.
             ( List.map [ to_caml; of_caml ] ~f:signature_src,
               List.map [ to_caml; of_caml ] ~f:impl_src ))
    |> List.unzip
  in
  let signatures = List.concat signatures |> nl_concat in
  let impls = List.concat impls |> nl_concat in
  ([ typedefs; signatures ] |> nl_concat, impls)

(* TODO:

   check that the compiler gives warning when passing a type that contains an
   unboxable type *)

let rts = ctypes |> List.map ~f:Schematic.C.roundtrip_src |> nl_concat

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
  Fmt.pr "// Generated declarations\n";
  Fmt.pr "//\n";
  Fmt.pr "%s" declarations;
  Fmt.pr "\n";
  Fmt.pr "//\n";
  Fmt.pr "// Generated implementations\n";
  Fmt.pr "//\n";
  Fmt.pr "%s" impls;
  Fmt.pr "\n";
  Fmt.pr "\n";
  Fmt.pr "%s" rts;
  ()

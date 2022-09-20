let json_of_value_callback_name = "schematic_c_json_string_of_value"
let json_to_value_callback_name = "schematic_c_json_string_to_value"

(* Globally register JSON conversion callbacks, to be invoked by our bundled C
   runtime. *)
let json_string_of_value (v : Ezjsonm.value) : string =
  Ezjsonm.value_to_string v

let json_string_to_value (s : string) : Ezjsonm.value =
  Ezjsonm.value_from_string s

let register_callbacks () =
  Stdlib.Callback.register json_of_value_callback_name json_string_of_value;
  Stdlib.Callback.register json_to_value_callback_name json_string_to_value;
  ()

let banners section_name =
  match
    [ "start"; "end" ]
    |> List.map (fun word ->
           Fmt.str "\n\n//\n// %s %s\n//\n\n" section_name word)
  with
  | [ s; e ] -> (s, e)
  | _ -> failwith "unreachable"

let bannerify section_name body =
  let start_banner, end_banner = banners section_name in
  start_banner ^ body ^ end_banner

let headers =
  bannerify "Schematic C backend support headers."
    {|
#define CAML_NAME_SPACE
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
|}

(* Can't say this looks beautiful, but it allows for flexibility in callback
   naming... *)

(* FIXME: null byte handling *)
let json_of_caml_builtin =
  {|
json json_of_caml(value v) {
  CAMLparam1(v);
  CAMLlocal1(caml_json_string);
  static const value* f = NULL;
  if (f == NULL) {
|}
  ^ Fmt.str "    f = caml_named_value(\"%s\");" json_of_value_callback_name
  ^ {|
  }
  caml_json_string = caml_callback(*f, v);
  char* json_string = strdup(String_val(caml_json_string));
  CAMLreturnT(json, json_string);
}
|}

(* FIXME broken for now *)
let json_to_caml_builtin =
  {|
value json_to_caml(char* x) {
  CAMLparam0();
  CAMLlocal1(v);
  
  v = caml_copy_string(x);
  
  static const value* f = NULL;
  if (f == NULL) {
|}
  ^ Fmt.str "    f = caml_named_value(\"%s\");" json_to_value_callback_name
  ^ {|
  }
  v = caml_callback(*f, v);

  CAMLreturn(v);
}
|}

let builtins =
  {|
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


value int64_t_to_caml(int64_t x) {
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_copy_int64(x);
  CAMLreturn(v);
}

int64_t int64_t_of_caml(value v) {
  CAMLparam1(v);
  int64_t x = Int64_val(v);
  CAMLreturnT(int64_t, x);
}

value bool_to_caml(bool x) {
  CAMLparam0();
  CAMLlocal1(v);
  v = Val_bool(x);
  CAMLreturn(v);
}

bool bool_of_caml(value v) {
  CAMLparam1(v);
  bool x = Bool_val(v);
  CAMLreturnT(bool, x);
}


double double_of_caml(value v) {
  CAMLparam1(v);
  double x = Double_val(v);
  CAMLreturnT(double, x);
}


value double_to_caml(double x) {
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_copy_double(x);
  CAMLreturn(v);
}

typedef char* json;
|}
  ^ json_of_caml_builtin ^ json_to_caml_builtin

let builtins = bannerify "Schematic C builtins." builtins
let runtime = headers ^ builtins

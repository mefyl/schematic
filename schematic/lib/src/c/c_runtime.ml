let headers =
  {|
//
// Schematic C backend support headers.
//

#define CAML_NAME_SPACE
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

//
// End Schematic support headers.
//

|}

let builtins =
  {|
//
// Schematic C builtins.
//
  
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

//
// End Schematic C builtins.
//

|}

let runtime = headers ^ builtins

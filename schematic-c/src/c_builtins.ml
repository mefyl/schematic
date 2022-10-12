open C_runtime
open C_pp

let poly_compare_symbol = "caml_poly_compare"

(* TODO: include guards *)

let headers =
  bannerify "Schematic C backend support headers."
    {|
#define CAML_NAME_SPACE

// C stdlib headers
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <time.h>

// OCaml headers.
// Ensure search path is consistent with actual object files!
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
sch_json sch_json_of_caml(value v) {
  CAMLparam1(v);
  CAMLlocal1(caml_json_string);
  static const value* f = NULL;
  if (f == NULL) {
|}
  ^ Fmt.str "    f = caml_named_value(\"%s\");" json_of_value_callback_name
  ^ {|
  }
  caml_json_string = caml_callback(*f, v);
  const char* json_string = strdup(String_val(caml_json_string));
  CAMLreturnT(sch_json, json_string);
}
|}

let json_to_caml_builtin =
  {|
value sch_json_to_caml(const char* x) {
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

let typedefs =
  {|
typedef int unit;


typedef struct {
    int days;
    // Picoseconds, in the [0; 86_399_999_999_999_999] range.
    int64_t ps;
} sch_time;

typedef const char* sch_json;
|}

(* TODO: DRY builtin sigs / impls. *)
let signatures =
  bannerify "Schematic C builtin signatures"
    {|
value int_to_caml(int x);
int int_of_caml(value x);
value string_to_caml(const char* p);
const char* string_of_caml(value v);
value int64_t_to_caml(int64_t x);
int64_t int64_t_of_caml(value v);
value bool_to_caml(bool x);
bool bool_of_caml(value v);
double double_of_caml(value v);
value double_to_caml(double x);

sch_time sch_time_of_caml(value v);
value sch_time_to_caml(sch_time x);

sch_time timespec_to_sch_time(struct timespec ts);
struct timespec timespec_of_sch_time(sch_time st);
|}

let implementations =
  bannerify "Schematic C builtin implementations"
  @@ {|

value unit_to_caml(unit x){
  CAMLparam0();
  // assert(x == 0);
  CAMLreturn(Val_unit);
}

unit unit_of_caml(value v) {
  CAMLparam1(v);
  // assert(v == Val_unit);
  CAMLreturnT(unit, 0);
}

value int_to_caml(int x) {
  CAMLparam0();
  CAMLreturn(Val_int(x));
}

int int_of_caml(value x) {
  CAMLparam1(x);
  CAMLreturnT(int, Int_val(x));
}

value string_to_caml(const char* p) {
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_copy_string(p);
  CAMLreturn(v);
}

const char* string_of_caml(value v) {
  CAMLparam1(v);
  const char* p = strdup(String_val(v));
  CAMLreturnT(const char*, p);
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


sch_time sch_time_of_caml(value v) {
  CAMLparam1(v);

  sch_time x = {
    .days = Long_val(Field(v, 0)),
    .ps = Int64_val(Field(v, 1))
  };

  CAMLreturnT(sch_time, x);
}


value sch_time_to_caml(sch_time x) {
  CAMLparam0();
  CAMLlocal1(v);

  v = caml_alloc_tuple(2);
  Store_field(v, 0, Val_long(x.days));
  Store_field(v, 1, caml_copy_int64(x.ps));
  CAMLreturn(v);
}



// Extras


static const int64_t PS_IN_S = 1000000000000;
static const int64_t PS_IN_NS = 1000;
static const int64_t S_IN_DAY = 86400;


struct timespec timespec_of_sch_time(sch_time st) {
  int64_t days_seconds = S_IN_DAY * st.days;
  int64_t ps_seconds = st.ps / PS_IN_S;
  int64_t rem_ps = st.ps % PS_IN_S;

  struct timespec x = {
      .tv_sec = days_seconds + ps_seconds,
      .tv_nsec = (rem_ps / PS_IN_NS)
  };

  return x;
}

sch_time timespec_to_sch_time(struct timespec ts) {
  long rem_s = ts.tv_sec % S_IN_DAY;

  sch_time st = {
    .days = ts.tv_sec / S_IN_DAY,
    .ps = (rem_s * PS_IN_S) + (ts.tv_nsec * PS_IN_NS)
  };

  return st;
}

|}
  ^ json_of_caml_builtin ^ json_to_caml_builtin

let declarations = headers ^ typedefs ^ signatures

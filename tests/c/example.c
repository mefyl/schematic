#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

typedef struct {
  int a;
  int b;
} primitive_record;

primitive_record* primitive_record_of_caml(value v) {
  CAMLparam1(v);

  // Allocate C value.
  primitive_record* p = malloc(sizeof(*p));

  // Read & convert OCaml fields.
  p->a = Int_val(Field(v, 0));
  p->b = Int_val(Field(v, 1));

  CAMLreturnT(primitive_record*, p);
}

// This DOES NOT free p.
value primitive_record_to_caml(primitive_record* p) {
  CAMLparam0();
  CAMLlocal1(v);

  // Allocate Caml value.
  int n_fields = 2;
  int tag = 0;
  v = caml_alloc(n_fields, tag);

  // Read & convert C fields.
  Store_field(v, 0, Val_int(p->a));
  Store_field(v, 1, Val_int(p->b));

  CAMLreturn(v);
}

void free_primitive_record(primitive_record* p) {
  // No inner allocated fields.

  // Only free outer.
  free(p);
}
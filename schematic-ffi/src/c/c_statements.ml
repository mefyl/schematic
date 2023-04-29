let field = Fmt.str "Field(%s, %d)"

let store ~dst ~field_idx ~field_val =
  Fmt.str "Store_field(%s, %d, %s)" dst field_idx field_val

let p0 = "CAMLparam0()"
let p1 = Fmt.str "CAMLparam1(%s)"
let l1 = Fmt.str "CAMLlocal1(%s)"
let l2 = Fmt.str "CAMLlocal2(%s, %s)"
let ln = Fmt.str "CAMLlocalN(%s, %d)"
let return_t = Fmt.str "CAMLreturnT(%s, %s)"
let return_val = Fmt.str "CAMLreturn(%s)"
let hash_variant = Fmt.str "caml_hash_variant(\"%s\")"
let val_long = Fmt.str "Val_long(%s)"
let c_assert = Fmt.str "assert(%s)"

let assert_block_size v n =
  [
    c_assert (Fmt.str "Is_block(%s)" v);
    Fmt.str
      {|if (Wosize_val(%s) != %d) { fprintf(stderr, "invalid block size: %%ld != %%d\\n", Wosize_val(%s), %d); assert(Wosize_val(%s) == %d); }|}
      v n v n v n;
  ]

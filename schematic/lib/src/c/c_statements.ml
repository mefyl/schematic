let field = Fmt.str "Field(%s, %d)"

let store ~dst ~field_idx ~field_val =
  Fmt.str "Store_field(%s, %d, %s)" dst field_idx field_val

let p0 = "CAMLparam0()"
let p1 = Fmt.str "CAMLparam1(%s)"
let return_t = Fmt.str "CAMLreturnT(%s, %s)"
let return_val = Fmt.str "CAMLreturn(%s)"

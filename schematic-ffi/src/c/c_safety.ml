open C_intf

let roundtrip_for = Fmt.str "sch_%a_rt_id" Identifier.pp

let roundtrip_src name =
  let open Fmt in
  let open C_format in
  let decl = Fmt.str "CAMLprim value %s(value v)" (roundtrip_for name) in
  let body =
    const pp_statements
      C_statements.
        [
          p1 "v";
          "CAMLlocal1(ret)";
          Fmt.str "%a x = %a(v)" Identifier.pp name Identifier.pp
            (of_caml_name_for name);
          Fmt.str "ret = %a(x)" Identifier.pp (to_caml_name_for name);
          "CAMLreturn(ret)";
        ]
  in
  let pp = block ~indent:2 decl body in
  Fmt.to_to_string pp ()

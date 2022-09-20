open Base
include C_intf

(* End of shameless copy *)

(* XXX: Butchering the boxing logic while I learn it. *)

include C_schemas

let export (name, c) : c_gen =
  let open Fmt in
  let of_caml, to_caml = C_conv.converters name c in
  { name; type_expr = const C_typeexpr.pp c; to_caml; of_caml }

let typedef_src { name; type_expr; _ } =
  let s = Fmt.to_to_string type_expr () in
  Fmt.str "typedef %s %s;\n" s name

let signature_src { signature; _ } = Fmt.str "%s;" signature

let impl_src { signature; impl } =
  Fmt.to_to_string (C_pp.braced ~indent:2 signature impl) ()

let free_src (_ : c_gen) = failwith "no free() yet"

let roundtrip_src (name, c) =
  let open Fmt in
  let open C_pp in
  (* Bit of a hack... *)
  let c = Atom (name, Some c) in
  let typ = Fmt.to_to_string C_typeexpr.pp c in

  let decl = Fmt.str "CAMLprim value sch_%s_rt_id(value v)" name in
  let body =
    const pp_statements
      [
        "CAMLparam1(v)";
        "CAMLlocal1(ret)";
        Fmt.str "%s x = %s(v)" typ (of_caml_name_for name);
        Fmt.str "ret = %s(x)" (to_caml_name_for name);
        "CAMLreturn(ret)";
      ]
  in
  let pp = braced ~indent:2 decl body in
  Fmt.to_to_string pp ()

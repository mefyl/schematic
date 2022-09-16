open Base
open C_intf

(* TODO: prefixes *)
let of_caml_name_for = Fmt.str "%s_of_caml"
let to_caml_name_for = Fmt.str "%s_to_caml"
let break sp id fmt _ = Caml.Format.pp_print_break fmt sp id
let force_newline fmt _ = Caml.Format.pp_force_newline fmt ()

let pp_of_caml_converter ?(indent = 2) (f : Stdlib.Format.formatter) (name, ast)
    =
  let open Fmt in
  let ptr_type = Fmt.str "%s*" name in
  let c_function_name = of_caml_name_for name in
  let (body : unit Fmt.t) =
    match ast with
    | Struct { fields } ->
      let field_stores =
        fields
        |> List.mapi ~f:(fun i (field_name, field_type) ->
               Fmt.pr "field_type: %a\n" C_intf.pp field_type;
               let conv_fn =
                 match field_type with
                 | Atom typename | Pointer (Atom typename) ->
                   of_caml_name_for typename
                 | _ -> failwith "unsupported nested type COIN"
               in
               Fmt.str "p->%s = %s(Field(v, %d))" field_name conv_fn i)
      in
      (* *)
      let statements =
        [ "CAMLparam1(v)"; Fmt.str "%s p = malloc(sizeof(*p))" ptr_type ]
        @ field_stores
        @ [ Fmt.str "CAMLreturnT(%s, p)" ptr_type ]
      in
      let statement = Fmt.fmt "%s" in
      const (vbox @@ list ~sep:semi statement) statements ++ const char ';'
    | Atom _s -> failwith "a"
    | Pointer _ -> failwith "ptr"
  in

  let decl = Fmt.str "%s %s(value v)" ptr_type c_function_name in
  let opening =
    hbox @@ (const string decl ++ const char ' ' ++ const char '{')
  in
  let printer =
    opening ++ break 0 2 ++ hvbox body ++ break 0 (-2) ++ const char '}'
  in
  printer f ()

let pp_to_caml_converter ?(indent = 2) (f : Stdlib.Format.formatter) (name, ast)
    =
  let open Fmt in
  let ptr_type = Fmt.str "%s*" name in
  let c_function_name = to_caml_name_for name in
  let (body : unit Fmt.t) =
    match ast with
    | Struct { fields } ->
      (* XXX: is this always the right tag? *)
      let tag = 0 in

      let n_fields = List.length fields in
      let field_stores =
        fields
        |> List.mapi ~f:(fun i (field_name, field_type) ->
               let conv_fn =
                 match field_type with
                 | Atom typename -> to_caml_name_for typename
                 | _ -> failwith "unsupported nested type"
               in
               Fmt.str "Store_field(v, %d, %s(p->%s))" i conv_fn field_name)
      in
      let statements =
        [
          "CAMLparam0()";
          "CAMLlocal1(v)";
          Fmt.str "int n_fields = %d" n_fields;
          Fmt.str "int tag = %d" tag;
          "v = caml_alloc(n_fields, tag)";
        ]
        @ field_stores @ [ "CAMLreturn(v)" ]
      in
      let statement = Fmt.fmt "%s" in
      const (vbox @@ list ~sep:semi statement) statements ++ const char ';'
    | Atom _s -> failwith "a"
    | Pointer _ -> failwith "ptr"
  in

  let decl = Fmt.str "value %s(%s p)" c_function_name ptr_type in
  let opening =
    hbox @@ (const string decl ++ const char ' ' ++ const char '{')
  in
  let printer =
    opening ++ break 0 2 ++ hvbox body ++ break 0 (-2) ++ const char '}'
  in
  printer f ()

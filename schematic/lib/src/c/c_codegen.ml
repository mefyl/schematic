open Base
open C_intf

(* TODO: prefixes *)
let of_caml_name_for = Fmt.str "%s_of_caml"
let to_caml_name_for = Fmt.str "%s_to_caml"

(* TODO: - variants (with or without payloads - separate representations)

   - polymorphic variants (need to hash - prehash? interaction with main?) *)

let pp_statements = Fmt.((vbox @@ list ~sep:semi string) ++ const char ';')
let ptr_type = Fmt.str "%s*"

let signature ~return_type ~fname ~argtype ~argname =
  Fmt.str "%s %s(%s %s)" return_type fname argtype argname

let pp_of_caml_converter ?(indent = 2) (f : Stdlib.Format.formatter) (name, ast)
    =
  let open Fmt in
  let ptr_type = ptr_type name in
  let c_function_name = of_caml_name_for name in
  let is_ptr, (body : unit Fmt.t) =
    match ast with
    | Struct { fields } ->
      let field_stores =
        fields
        |> List.mapi ~f:(fun i (field_name, field_type) ->
               Fmt.epr "field_type: %a\n" C_intf.pp field_type;
               let conv_fn =
                 match field_type with
                 | Pointer (Atom "char") -> of_caml_name_for "string"
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
      (true, const pp_statements statements)
    | Variant (_name, constructors) ->
      let plain_ctrs, rich_ctrs =
        List.partition_map
          ~f:(function (_, Void) as x -> First x | (_, _) as x -> Second x)
          constructors
      in

      let plain_case =
        (* TODO: add assertion *)
        let stmts =
          [
            Fmt.str "%s x = Int_val(v)" name;
            Fmt.str "assert(x < %d)" (List.length plain_ctrs);
            "return x";
          ]
        in
        const pp_statements stmts
      in

      if List.is_empty rich_ctrs then (false, plain_case)
      else
        let rich_case = const string "RICH" in
        let open Fmt in
        ( true,
          vbox
            (vbox ~indent (const string "if (Is_long(v)) {" ++ cut ++ plain_case)
            ++ cut
            ++ vbox ~indent (const string "} else {" ++ cut ++ rich_case)
            ++ cut ++ const char '}') )
    | x ->
      Fmt.epr "unsupported in of_caml: %a\n" pp x;
      failwith "unsupported"
  in
  let return_type = if is_ptr then ptr_type else name in
  let decl =
    signature ~return_type ~fname:c_function_name ~argtype:"value" ~argname:"v"
  in
  let opening =
    hbox @@ (const string decl ++ const char ' ' ++ const char '{')
  in
  let printer =
    vbox ~indent (opening ++ cut ++ hvbox body) ++ cut ++ const char '}'
  in
  printer f ()

let pp_to_caml_converter ?(indent = 2) (f : Stdlib.Format.formatter) (name, ast)
    =
  let open Fmt in
  let ptr_type = ptr_type name in
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
                 | Pointer (Atom "char") -> to_caml_name_for "string"
                 | Atom typename | Pointer (Atom typename) ->
                   to_caml_name_for typename
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
      const pp_statements statements
    | x ->
      Fmt.epr "unsupported in to_caml: %a\n" pp x;
      failwith "unsupported"
  in

  let decl = Fmt.str "value %s(%s p)" c_function_name ptr_type in
  let opening =
    hbox @@ (const string decl ++ const char ' ' ++ const char '{')
  in
  let printer =
    vbox ~indent (opening ++ cut ++ hvbox body) ++ cut ++ const char '}'
  in
  printer f ()

(* TODO: generate *_free() functions *)

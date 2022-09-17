open Base
open C_intf

(* TODO: prefixes *)
let of_caml_name_for = Fmt.str "%s_of_caml"
let to_caml_name_for = Fmt.str "%s_to_caml"
let force_newline fmt _ = Caml.Format.pp_force_newline fmt ()

(* TODO:

   - polymorphic variants (need to hash - prehash? interaction with main?) *)

let pp_statements = Fmt.((vbox @@ list ~sep:semi string) ++ const char ';')
let ptr_type = Fmt.str "%s*"

let signature ~return_type ~fname ~argtype ~argname =
  Fmt.str "%s %s(%s %s)" return_type fname argtype argname

(** Separate a variant's basic (LHS) and rich (RHS) constructors *)
let basic_rich_split =
  List.partition_map ~f:(function
    | (_, Void) as x -> First x
    | (_, _) as x -> Second x)

(* TODO: use the generic braced we wanted *)
let case ~indent case contents =
  let open Fmt in
  let post_case, post_box =
    match contents with
    | Some contents -> (cut ++ contents, nop)
    | None -> (nop, force_newline)
  in
  vbox ~indent (const string (Fmt.str "case %s:" case) ++ post_case) ++ post_box

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
      let statements =
        [ "CAMLparam1(v)"; Fmt.str "%s p = malloc(sizeof(*p))" ptr_type ]
        @ field_stores
        @ [ Fmt.str "CAMLreturnT(%s, p)" ptr_type ]
      in
      (true, const pp_statements statements)
    | Variant (_name, constructors) ->
      let plain_ctrs, rich_ctrs = basic_rich_split constructors in

      if List.is_empty rich_ctrs then
        let plain_case =
          let stmts =
            [
              Fmt.str "%s x = Int_val(v)" name;
              Fmt.str "assert(x < %d)" (List.length plain_ctrs);
              "return x";
            ]
          in
          const pp_statements stmts
        in
        (false, plain_case)
      else
        let plain_case =
          const pp_statements
            [
              "res.tag = Int_val(v)";
              Fmt.str "assert(res.tag < %d)" (List.length plain_ctrs);
            ]
        in
        let case : (int * (string * c)) Fmt.t =
         fun fmt (i, (field_name, c)) ->
          let typename =
            match c with Atom s -> s | _ -> failwith "unsupported"
          in
          case ~indent (Int.to_string i)
            (Some
               (const pp_statements
                  [
                    (* XXX: tuples will ruin the day. *)
                    (* FIXME FIXME FIXME *)
                    Fmt.str "res.payload.%s = %s(Field(v, 0))" field_name
                      (of_caml_name_for typename);
                    "break";
                  ]))
            fmt ()
        in

        let rich_case =
          let switch =
            vbox ~indent
              (const string "switch (Tag_val(v)) {"
              ++ cut
              ++ const (list ~sep:semi case)
                   (rich_ctrs
                   (* XXX: this must exist already? :( )*)
                   |> List.mapi ~f:(fun i x -> (i, x))))
            ++ cut ++ const char '}'
          in
          const pp_statements
            [
              Fmt.str "res.tag = %d + Tag_val(v)" (List.length plain_ctrs);
              Fmt.str "assert(Tag_val(v) < %d)" (List.length rich_ctrs);
            ]
          ++ cut ++ switch
        in

        let open Fmt in
        ( false,
          vbox
            (const pp_statements [ "CAMLparam1(v)"; Fmt.str "%s res" name ]
            ++ cut
            ++ vbox ~indent
                 (const string "if (Is_long(v)) {" ++ cut ++ plain_case)
            ++ cut
            ++ vbox ~indent (const string "} else {" ++ cut ++ rich_case)
            ++ cut ++ const char '}')
          ++ cut
          ++ const pp_statements [ Fmt.str "CAMLreturnT(%s, res)" name ] )
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
    vbox ~indent (opening ++ cut ++ vbox body) ++ cut ++ const char '}'
  in
  printer f ()

let pp_to_caml_converter ?(indent = 2) (f : Stdlib.Format.formatter) (name, ast)
    =
  let open Fmt in
  let ptr_type = ptr_type name in
  let c_function_name = to_caml_name_for name in
  let argname, is_ptr, (body : unit Fmt.t) =
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
      ("p", true, const pp_statements statements)
    | Variant (variant_name, constructors) ->
      let basic_ctrs, rich_ctrs = basic_rich_split constructors in
      let pp =
        if List.is_empty rich_ctrs then
          const pp_statements
            [
              Fmt.str "assert(x < %d)" (List.length basic_ctrs);
              "return Val_int(x)";
            ]
        else
          let basic_cases =
            basic_ctrs
            |> List.mapi ~f:(fun i (constr_name, _) ->
                   let case_name =
                     Fmt.str "%s_%s_tag" variant_name constr_name
                   in
                   let case_contents =
                     const pp_statements
                       [
                         (* XXX FIXME alloc size - tuples...*)
                         Fmt.str "v = Val_int(%d)" i;
                         "break";
                       ]
                   in
                   case ~indent case_name (Some case_contents))
          in

          let rich_cases =
            rich_ctrs
            |> List.mapi ~f:(fun i (constr_name, constr_type) ->
                   let case_name =
                     Fmt.str "%s_%s_tag" variant_name constr_name
                   in
                   let constr_type =
                     match constr_type with
                     | Atom s -> s
                     | _ -> failwith "unsupported"
                   in
                   let conv_fn_name = to_caml_name_for constr_type in
                   let case_contents =
                     const pp_statements
                       [
                         (* XXX FIXME alloc size - tuples*)
                         Fmt.str "v = caml_alloc(1, %d)" i;
                         Fmt.str "Store_field(v, 0, %s(x.payload.%s))"
                           conv_fn_name constr_name;
                         "break";
                       ]
                   in
                   case ~indent case_name (Some case_contents))
          in

          let cases =
            basic_cases @ rich_cases
            |> List.fold ~init:nop ~f:(fun acc x -> acc ++ cut ++ x)
          in

          let switch =
            vbox ~indent (const string "switch (x.tag) {" ++ cases)
            ++ cut ++ const char '}'
          in

          const pp_statements [ "CAMLparam0()"; "CAMLlocal1(v)" ]
          ++ cut ++ switch ++ cut
          ++ const pp_statements [ "CAMLreturn(v)" ]
      in
      ("x", false, pp)
    | x ->
      Fmt.epr "unsupported in to_caml: %a\n" pp x;
      failwith "unsupported"
  in
  let argtype = if is_ptr then ptr_type else name in
  let decl =
    signature ~return_type:"value" ~fname:c_function_name ~argtype ~argname
  in

  let opening =
    hbox @@ (const string decl ++ const char ' ' ++ const char '{')
  in
  let printer =
    vbox ~indent (opening ++ cut ++ vbox body) ++ cut ++ const char '}'
  in
  printer f ()

(* TODO: generate *_free() functions *)

open Base
open C_intf
open C_pp

(* TODO: prefixes *)
(* TODO:

   - polymorphic variants (need to hash - prehash? interaction with main?) *)

let force_newline fmt _ = Caml.Format.pp_force_newline fmt ()
let pp_statements = Fmt.((vbox @@ list ~sep:semi string) ++ const char ';')
let ptr_type = Fmt.str "%s*"

let signature ~return_type ~fname ~argtype ~argname =
  Fmt.str "%s %s(%s %s)" return_type fname argtype argname

(* TODO: use the generic braced *)
let case ~indent case contents =
  let open Fmt in
  let post_case, post_box =
    match contents with
    | Some contents -> (cut ++ contents, nop)
    | None -> (nop, force_newline)
  in
  vbox ~indent (const string (Fmt.str "case %s:" case) ++ post_case) ++ post_box

let pp_of_caml_converter ?(indent = 2) (name, ast) =
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
               let conv_fn = of_caml_name_for_c field_type in
               Fmt.str "p->%s = %s(Field(v, %d))" field_name conv_fn i)
      in
      let statements =
        [ "CAMLparam1(v)"; Fmt.str "%s p = malloc(sizeof(*p))" ptr_type ]
        @ field_stores
        @ [ Fmt.str "CAMLreturnT(%s, p)" ptr_type ]
      in
      (true, const pp_statements statements)
      (* Plain-only variant *)
    | Variant { basic; rich = []; _ } ->
      let pp =
        const pp_statements
          [
            Fmt.str "%s x = Int_val(v)" name;
            Fmt.str "assert(x < %d)" (List.length basic);
            "return x";
          ]
      in
      (false, pp)
    | Variant { basic; rich; _ } ->
      let plain_case =
        const pp_statements
          [
            "res.tag = Int_val(v)";
            Fmt.str "assert(res.tag < %d)" (List.length basic);
          ]
      in
      let case : (int * (string * c)) Fmt.t =
       fun fmt (i, (field_name, c)) ->
        case ~indent (Int.to_string i)
          (Some
             (const pp_statements
                [
                  (* XXX: tuples will ruin the day. *)
                  (* FIXME FIXME FIXME *)
                  Fmt.str "res.payload->%s = %s(Field(v, 0))" field_name
                    (of_caml_name_for_c c);
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
                 (rich
                 (* XXX: this must exist already? :( )*)
                 |> List.mapi ~f:(fun i x -> (i, x))))
          ++ cut ++ const char '}'
        in
        const pp_statements
          [
            Fmt.str "res.tag = %d + Tag_val(v)" (List.length basic);
            "res.payload = malloc(sizeof(*res.payload))";
            Fmt.str "assert(Tag_val(v) < %d)" (List.length rich);
          ]
        ++ cut ++ switch
      in

      let open Fmt in
      ( false,
        vbox
          (const pp_statements [ "CAMLparam1(v)"; Fmt.str "%s res" name ]
          ++ cut
          ++ vbox ~indent (const string "if (Is_long(v)) {" ++ cut ++ plain_case)
          ++ cut
          ++ vbox ~indent (const string "} else {" ++ cut ++ rich_case)
          ++ cut ++ const char '}')
        ++ cut
        ++ const pp_statements [ Fmt.str "CAMLreturnT(%s, res)" name ] )
    | Tuple members ->
      let tuptype = tuple_name members in
      let pp =
        const pp_statements
        @@ [ "CAMLparam1(v)"; Fmt.str "%s res" tuptype ]
        @ (members
          |> List.mapi ~f:(fun i c ->
                 Fmt.str "res.t%d = %s(Field(v, %d))" i (of_caml_name_for_c c) i)
          )
        @ [ Fmt.str "CAMLreturnT(%s, res)" tuptype ]
      in
      (false, pp)
    | Array elem_type ->
      let length_while =
        braced ~indent "while (l != Val_emptylist)"
          (const pp_statements [ "l = Field(l, 1)"; "arr.len++" ])
      in
      let set_for =
        braced ~indent "for (int64_t i = 0; i < arr.len; i++)"
          (const pp_statements
             [
               Fmt.str "arr.data[i] = %s(Field(l, 0))"
                 (of_caml_name_for_c elem_type);
               "l = Field(l, 1)";
             ])
      in
      let pp =
        const pp_statements
          [
            "CAMLparam1(v)";
            "CAMLlocal1(l)";
            Fmt.str "%s arr = {.len = 0, .data = NULL}" (array_name elem_type);
            "l = v";
          ]
        ++ cut ++ length_while ++ cut
        ++ const pp_statements
             [ "l = v"; "arr.data = malloc(sizeof(*arr.data) * arr.len)" ]
        ++ cut ++ set_for ++ cut
        ++ const pp_statements
             [ Fmt.str "CAMLreturnT(%s, arr)" (array_name elem_type) ]
      in

      (false, pp)
    (* The considered type is merely an alias for another *)
    | (Atom _ | String) as c ->
      let pp =
        const pp_statements
          [
            "CAMLparam1(v)";
            Fmt.str "%a res = %s(v)" (C_typeexpr.pp ~indent) c
              (of_caml_name_for_c c);
            Fmt.str "CAMLreturnT(%s, res)" name;
          ]
      in
      (false, pp)
    | _ -> failwith "no"
  in
  let return_type = if is_ptr then ptr_type else name in
  let decl =
    signature ~return_type ~fname:c_function_name ~argtype:"value" ~argname:"v"
  in
  { signature = decl; impl = body }

let pp_to_caml_converter ?(indent = 2) (name, ast) =
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
               let conv_fn = to_caml_name_for_c field_type in
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
    (* Plain-only variant *)
    | Variant { basic; rich = []; _ } ->
      let pp =
        const pp_statements
          [ Fmt.str "assert(x < %d)" (List.length basic); "return Val_int(x)" ]
      in
      ("x", false, pp)
    | Variant { name = variant_name; basic; rich } ->
      let pp =
        let basic_cases : (string * unit Fmt.t) list =
          basic
          |> List.mapi ~f:(fun i constr_name ->
                 ( constr_name,
                   const pp_statements
                     [
                       (* XXX FIXME alloc size - tuples...*)
                       Fmt.str "v = Val_int(%d)" i;
                       "break";
                     ] ))
        in
        let rich_cases : (string * unit Fmt.t) list =
          rich
          |> List.mapi ~f:(fun i (constr_name, constr_type) ->
                 ( constr_name,
                   const pp_statements
                     [
                       (* XXX FIXME alloc size - tuples*)
                       Fmt.str "v = caml_alloc(1, %d)" i;
                       Fmt.str "Store_field(v, 0, %s(x.payload->%s))"
                         (to_caml_name_for_c constr_type)
                         constr_name;
                       "break";
                     ] ))
        in
        let cases =
          basic_cases @ rich_cases
          |> List.map ~f:(fun (constr_name, contents) ->
                 case ~indent
                   (case_name variant_name constr_name)
                   (Some contents))
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
    | Tuple members ->
      let pp =
        const pp_statements
        @@ [
             "CAMLparam0()";
             "CAMLlocal1(v)";
             Fmt.str "v = caml_alloc_tuple(%d)" (List.length members);
           ]
        @ (members
          |> List.mapi ~f:(fun i c ->
                 let conv = to_caml_name_for_c c in
                 Fmt.str "Store_field(v, %d, %s(x.t%d))" i conv i))
        @ [ "CAMLreturn(v)" ]
      in
      ("x", false, pp)
    | Array elem_type ->
      let loop =
        braced ~indent "for (int64_t i = arr.len - 1; i >= 0; i--)"
          (const pp_statements
             [
               "cons = caml_alloc(2, 0)";
               "typeof(*arr.data) x = arr.data[i]";
               Fmt.str "Store_field(cons, 0, %s(x))"
                 (to_caml_name_for_c elem_type);
               "Store_field(cons, 1, cli)";
               "cli = cons";
             ])
      in
      let pp =
        const pp_statements
          [ "CAMLparam0()"; "CAMLlocal2(cli, cons)"; "cli = Val_emptylist" ]
        ++ cut ++ loop ++ cut
        ++ const pp_statements [ "CAMLreturn(cli)" ]
      in

      ("arr", false, pp)
    | (Atom _ | String) as c ->
      let pp =
        const pp_statements
          [
            "CAMLparam0()";
            "CAMLlocal1(v)";
            Fmt.str "v = %s(x)" (to_caml_name_for_c c);
            "CAMLreturn(v)";
          ]
      in
      ("x", false, pp)
    | x -> failwith "unsupported in to_caml: %a\n" pp x
  in
  let argtype = if is_ptr then ptr_type else name in
  let decl =
    signature ~return_type:"value" ~fname:c_function_name ~argtype ~argname
  in
  { signature = decl; impl = body }

(* TODO: generate *_free() functions *)

let converters name ast =
  let open Fmt in
  (pp_of_caml_converter (name, ast), pp_to_caml_converter (name, ast))

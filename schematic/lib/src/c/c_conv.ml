open Base
open C_intf
open C_pp

(* TODO: prefixes *)
(* TODO:

   - polymorphic variants (need to hash - prehash? interaction with main?) *)

let force_newline fmt _ = Caml.Format.pp_force_newline fmt ()

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

let pp_of_caml_converter ?(safety_checks = false) ?(indent = 2)
    ((name : Identifier.t), ast) =
  let open Fmt in
  let ptr_type = ptr_type (Identifier.to_string name) in
  let c_function_name = of_caml_name_for name in
  let is_ptr, (body : unit Fmt.t) =
    match ast with
    | Struct { fields } ->
      let field_stores =
        fields
        |> List.mapi ~f:(fun i (field_name, field_type) ->
               Logs.debug (fun m -> m "field_type: %a\n" C_intf.pp field_type);
               let conv_fn = of_caml_name_for_c field_type in
               Fmt.str "p->%a = %a(Field(v, %d))" Identifier.pp field_name
                 Identifier.pp conv_fn i)
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
            Fmt.str "%a x = Int_val(v)" Identifier.pp name;
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
      let case : (int * (Identifier.t * c)) Fmt.t =
       fun fmt (i, (field_name, c)) ->
        let stmts =
          match c with
          | Atom (_, Some (Tuple _)) ->
            [
              Fmt.str "res.payload->%a = %a(v)" Identifier.pp field_name
                Identifier.pp (of_caml_name_for_c c);
            ]
          | Struct { fields } ->
            fields
            |> List.mapi ~f:(fun i (inline_field_name, field_c) ->
                   Fmt.str "res.payload->%a.%a = %a(Field(v, %d))" Identifier.pp
                     field_name Identifier.pp inline_field_name Identifier.pp
                     (of_caml_name_for_c field_c)
                     i)
          | _ ->
            [
              Fmt.str "res.payload->%a = %a(Field(v, 0))" Identifier.pp
                field_name Identifier.pp (of_caml_name_for_c c);
            ]
        in

        Logs.debug (fun m -> m "Case type %a\n" pp c);
        case ~indent (Int.to_string i)
          (Some (const pp_statements (stmts @ [ "break" ])))
          fmt ()
      in

      let rich_case =
        let switch =
          vbox ~indent
            (const string "switch (Tag_val(v)) {"
            ++ cut
            ++ const (list ~sep:cut case)
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
          (const pp_statements
             [ "CAMLparam1(v)"; Fmt.str "%a res" Identifier.pp name ]
          ++ cut
          ++ vbox ~indent (const string "if (Is_long(v)) {" ++ cut ++ plain_case)
          ++ cut
          ++ vbox ~indent (const string "} else {" ++ cut ++ rich_case)
          ++ cut ++ const char '}')
        ++ cut
        ++ const pp_statements
             [ Fmt.str "CAMLreturnT(%a, res)" Identifier.pp name ] )
    | Tuple members ->
      let tuptype = tuple_name members in
      let pp =
        const pp_statements
        @@ [ "CAMLparam1(v)"; Fmt.str "%a res" Identifier.pp tuptype ]
        @ (members
          |> List.mapi ~f:(fun i c ->
                 Fmt.str "res.t%d = %a(Field(v, %d))" i Identifier.pp
                   (of_caml_name_for_c c) i))
        @ [ Fmt.str "CAMLreturnT(%a, res)" Identifier.pp tuptype ]
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
               Fmt.str "arr.data[i] = %a(Field(l, 0))" Identifier.pp
                 (of_caml_name_for_c elem_type);
               "l = Field(l, 1)";
             ])
      in
      let pp =
        const pp_statements
          [
            "CAMLparam1(v)";
            "CAMLlocal1(l)";
            Fmt.str "%a arr = {.len = 0, .data = NULL}" Identifier.pp
              (array_name elem_type);
            "l = v";
          ]
        ++ cut ++ length_while ++ cut
        ++ const pp_statements
             [ "l = v"; "arr.data = malloc(sizeof(*arr.data) * arr.len)" ]
        ++ cut ++ set_for ++ cut
        ++ const pp_statements
             [
               Fmt.str "CAMLreturnT(%a, arr)" Identifier.pp
                 (array_name elem_type);
             ]
      in

      (false, pp)
    (* The considered type is merely an alias for another *)
    | (Atom _ | String) as c ->
      let pp =
        const pp_statements
          [
            "CAMLparam1(v)";
            Fmt.str "%a res = %a(v)" (C_typeexpr.pp ~indent) c Identifier.pp
              (of_caml_name_for_c c);
            Fmt.str "CAMLreturnT(%a, res)" Identifier.pp name;
          ]
      in
      (false, pp)
    | _ -> failwith "no"
  in

  let safety_checks =
    if safety_checks then
      let checking, checked = checking_checked_for name in
      Fmt.str
        "if (!%s && !%s) {\n\
        \    %s = true;\n\
        \    // TODO add structural equality assertion\n\
        \    %s = true;\n\
        \  }" checking checked checking checked
    else ""
  in

  let body = const string safety_checks ++ body in

  let return_type = if is_ptr then ptr_type else Identifier.to_string name in
  let decl =
    signature ~return_type
      ~fname:(Identifier.to_string c_function_name)
      ~argtype:"value" ~argname:"v"
  in
  { signature = decl; impl = body }

let pp_to_caml_converter ?(indent = 2) ((name : Identifier.t), ast) =
  let open Fmt in
  let ptr_type = ptr_type (Identifier.to_string name) in
  let c_function_name = to_caml_name_for name in
  let argname, is_ptr, (body : unit Fmt.t) =
    match ast with
    | Struct { fields } ->
      let tag = 0 in
      let n_fields = List.length fields in
      let field_stores =
        fields
        |> List.mapi ~f:(fun i (field_name, field_type) ->
               let conv_fn = to_caml_name_for_c field_type in
               Fmt.str "Store_field(v, %d, %a(p->%a))" i Identifier.pp conv_fn
                 Identifier.pp field_name)
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
                 ( Identifier.to_string constr_name,
                   const pp_statements [ Fmt.str "v = Val_int(%d)" i; "break" ]
                 ))
        in
        let rich_cases : (string * unit Fmt.t) list =
          rich
          |> List.mapi ~f:(fun i (constr_name, constr_type) ->
                 Logs.debug (fun m ->
                     m "Rich case constructor type: %a" pp constr_type);
                 let payload =
                   Fmt.str "x.payload->%a" Identifier.pp constr_name
                 in
                 let n_fields, stmts =
                   match constr_type with
                   (* Inline tuple *)
                   | Atom (_, Some (Tuple members)) ->
                     ( members |> List.length,
                       members
                       |> List.mapi ~f:(fun i c ->
                              Fmt.str "Store_field(v, %d, %a(%s.t%d))" i
                                Identifier.pp (to_caml_name_for_c c) payload i)
                     )
                   (* Inline record *)
                   | Struct { fields } ->
                     ( fields |> List.length,
                       fields
                       |> List.mapi ~f:(fun i (field_name, c) ->
                              Fmt.str "Store_field(v, %d, %a(%s.%a))" i
                                Identifier.pp (to_caml_name_for_c c) payload
                                Identifier.pp field_name) )
                   | _ ->
                     ( 1,
                       [
                         Fmt.str "Store_field(v, 0, %a(%s))" Identifier.pp
                           (to_caml_name_for_c constr_type)
                           payload;
                       ] )
                 in
                 let tag = i in
                 ( Identifier.to_string constr_name,
                   const pp_statements
                     ([ Fmt.str "v = caml_alloc(%d, %i)" n_fields tag ]
                     @ stmts @ [ "break" ]) ))
        in
        let cases =
          basic_cases @ rich_cases
          |> List.map ~f:(fun (constr_name, contents) ->
                 case ~indent
                   (case_name (Identifier.to_string variant_name) constr_name)
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
                 Fmt.str "Store_field(v, %d, %a(x.t%d))" i Identifier.pp conv i)
          )
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
               Fmt.str "Store_field(cons, 0, %a(x))" Identifier.pp
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
            Fmt.str "v = %a(x)" Identifier.pp (to_caml_name_for_c c);
            "CAMLreturn(v)";
          ]
      in
      ("x", false, pp)
    | x -> failwith "unsupported in to_caml: %a\n" pp x
  in
  let argtype = if is_ptr then ptr_type else Identifier.to_string name in
  let decl =
    signature ~return_type:"value"
      ~fname:(Identifier.to_string c_function_name)
      ~argtype ~argname
  in
  { signature = decl; impl = body }

(* TODO: generate *_free() functions *)

let converters name ast =
  (pp_of_caml_converter (name, ast), pp_to_caml_converter (name, ast))

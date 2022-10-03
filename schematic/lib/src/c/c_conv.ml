open Base
open C_intf
open C_pp
open C_statements

let force_newline fmt _ = Caml.Format.pp_force_newline fmt ()

let signature ~return_type ~fname ~argtype ~argname =
  Fmt.str "%s %a(%s %s)" return_type Identifier.pp fname argtype argname

let case ~indent case contents =
  let open Fmt in
  let post_case, post_box =
    match contents with
    | Some contents -> (cut ++ contents, nop)
    | None -> (nop, force_newline)
  in
  vbox ~indent (const string (Fmt.str "case %s:" case) ++ post_case) ++ post_box

let cases ~indent l =
  let n = List.length l in
  let open Fmt in
  l
  |> List.map ~f:(fun (condition, contents) -> case ~indent condition contents)
  |> List.foldi ~init:nop ~f:(fun i acc x ->
         let post = if i = n - 1 then nop else cut in
         acc ++ x ++ post)

let pp_of_caml_converter ?(safety_checks = false) ?(indent = 2)
    ((name : Identifier.t), ast) =
  let open Fmt in
  let ptr_type = ptr_type (Identifier.to_string name) in
  let kind, (body : unit Fmt.t) =
    let open C_statements in
    match ast with
    | Struct { fields } ->
      let field_stores =
        fields
        |> List.mapi ~f:(fun field_idx (field_name, field_type) ->
               Logs.debug (fun m -> m "field_type: %a\n" C_intf.pp field_type);
               let conv_fn = of_caml_name_for_c field_type in
               Fmt.str "p->%a = %a(%s)" Identifier.pp field_name Identifier.pp
                 conv_fn (field "v" field_idx))
      in
      let statements =
        [ p1 "v"; Fmt.str "%s p = malloc(sizeof(*p))" ptr_type ]
        @ field_stores
        @ [ return_t ptr_type "p" ]
      in
      (`Pointer, const pp_statements statements)
      (* Plain-only regular variant *)
    | Variant { basic; rich = []; hashes = []; _ } ->
      let pp =
        const pp_statements
          [
            Fmt.str "%a x = Int_val(v)" Identifier.pp name;
            Fmt.str "assert(x < %d)" (List.length basic);
            "return x";
          ]
      in
      (`Plain, pp)
      (* Regular variant with payloads *)
    | Variant { basic; rich; hashes = []; _ } ->
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
                   Fmt.str "res.payload->%a.%a = %a(%s)" Identifier.pp
                     field_name Identifier.pp inline_field_name Identifier.pp
                     (of_caml_name_for_c field_c)
                     (field "v" i))
          | _ ->
            [
              Fmt.str "res.payload->%a = %a(%s)" Identifier.pp field_name
                Identifier.pp (of_caml_name_for_c c) (field "v" 0);
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
                 (* This function must exist already? :( *)
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
      ( `Plain,
        vbox
          (const pp_statements [ p1 "v"; Fmt.str "%a res" Identifier.pp name ]
          ++ cut
          ++ vbox ~indent (const string "if (Is_long(v)) {" ++ cut ++ plain_case)
          ++ cut
          ++ vbox ~indent (const string "} else {" ++ cut ++ rich_case)
          ++ cut ++ const char '}')
        ++ cut
        ++ const pp_statements [ return_t (Identifier.to_string name) "res" ] )
    (* Polymorphic variant *)
    | Variant { basic; rich; hashes; _ } ->
      let basic_only = List.is_empty rich in
      let tag_suffix = if basic_only then "" else "_tag" in
      let get_hash = List.Assoc.find_exn hashes ~equal:Identifier.equal in
      let elses =
        let basic =
          const (list ~sep:cut string)
            (basic
            |> List.map ~f:(fun id ->
                   let hash = get_hash id in
                   let tag = if basic_only then "x" else "x.tag" in
                   Fmt.str "else if (v == Val_long(%d)) %s = %a_%a%s;" hash tag
                     Identifier.pp name Identifier.pp id tag_suffix))
        in
        if basic_only then basic
        else
          let elses =
            let pp_rich : (Identifier.t * c) Fmt.t =
             fun fmt (id, constr_c) ->
              let hash = get_hash id in
              let elseif = Fmt.str "else if (tag == Val_long(%d))" hash in
              let body =
                const pp_statements
                  [
                    Fmt.str "x.tag = %a_%a_tag" Identifier.pp name Identifier.pp
                      id;
                    Fmt.str "x.payload->%a = %a(Field(v, 1))" Identifier.pp id
                      Identifier.pp
                      (of_caml_name_for_c constr_c);
                  ]
              in
              (braced ~indent elseif body) fmt ()
            in
            const (list ~sep:cut pp_rich) rich
          in

          let body =
            const pp_statements
              [
                "int tag = Field(v, 0)";
                "x.payload = malloc(sizeof(*x.payload))";
              ]
            ++ cut
            ++ (const string "if (0) {}" ++ cut ++ elses ++ cut
               ++ const string "else assert(0);")
          in

          let rich = braced ~indent "else if (Is_block(v))" body in
          basic ++ cut ++ rich
      in
      let if_else_chain =
        const string "if (0) {}" ++ cut ++ elses ++ cut
        ++ const string "else assert(0);"
      in
      let pp =
        const pp_statements [ p1 "v"; Fmt.str "%a x" Identifier.pp name ]
        ++ cut ++ if_else_chain ++ cut
        ++ const pp_statements [ return_t (Identifier.to_string name) "x" ]
      in
      (`Plain, pp)
    | Tuple members ->
      let tuptype = tuple_name members in
      let pp =
        const pp_statements
        @@ [ p1 "v"; Fmt.str "%a res" Identifier.pp tuptype ]
        @ (members
          |> List.mapi ~f:(fun field_idx c ->
                 Fmt.str "res.t%d = %a(%s)" field_idx Identifier.pp
                   (of_caml_name_for_c c) (field "v" field_idx)))
        @ [ return_t (Identifier.to_string tuptype) "res" ]
      in
      (`Plain, pp)
    | Array elem_type ->
      let length_while =
        braced ~indent "while (l != Val_emptylist)"
          (const pp_statements [ Fmt.str "l = %s" (field "l" 1); "arr.len++" ])
      in
      let set_for =
        braced ~indent "for (int64_t i = 0; i < arr.len; i++)"
          (const pp_statements
             [
               Fmt.str "arr.data[i] = %a(%s)" Identifier.pp
                 (of_caml_name_for_c elem_type)
                 (field "l" 0);
               Fmt.str "l = %s" (field "l" 1);
             ])
      in
      let pp =
        const pp_statements
          [
            p1 "v";
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
             [ return_t (Identifier.to_string (array_name elem_type)) "arr" ]
      in
      (`Plain, pp)
    (* The considered type is merely an alias for another *)
    | (Atom _ | String) as c ->
      let pp =
        const pp_statements
          [
            p1 "v";
            Fmt.str "%a res = %a(v)" (C_typeexpr.pp ~indent) c Identifier.pp
              (of_caml_name_for_c c);
            return_t (Identifier.to_string name) "res";
          ]
      in
      (`Plain, pp)
    | Void ->
      failwith "Void may not be instantiated, and therefore cannot be converted"
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
  let return_type =
    match kind with `Pointer -> ptr_type | `Plain -> Identifier.to_string name
  in
  let decl =
    signature ~return_type ~fname:(name |> of_caml_name_for) ~argtype:"value"
      ~argname:"v"
  in
  { signature = decl; impl = body }

let pp_to_caml_converter ?(indent = 2) ((name : Identifier.t), ast) =
  let open Fmt in
  let ptr_type = ptr_type (Identifier.to_string name) in
  let argname, kind, (body : unit Fmt.t) =
    match ast with
    | Struct { fields } ->
      let tag = 0 in
      let n_fields = List.length fields in
      let field_stores =
        fields
        |> List.mapi ~f:(fun field_idx (field_name, field_type) ->
               let conv_fn = to_caml_name_for_c field_type in
               let field_val =
                 Fmt.str "%a(p->%a)" Identifier.pp conv_fn Identifier.pp
                   field_name
               in
               store ~dst:"v" ~field_idx ~field_val)
      in
      let statements =
        [
          p0;
          "CAMLlocal1(v)";
          Fmt.str "int n_fields = %d" n_fields;
          Fmt.str "int tag = %d" tag;
          "v = caml_alloc(n_fields, tag)";
        ]
        @ field_stores
        @ [ return_val "v" ]
      in
      ("p", `Pointer, const pp_statements statements)
    (* Plain-only regular variant *)
    | Variant { basic; rich = []; hashes = []; _ } ->
      let pp =
        const pp_statements
          [ Fmt.str "assert(x < %d)" (List.length basic); "return Val_int(x)" ]
      in
      ("x", `Plain, pp)
    (* Regular variant with payloads *)
    | Variant { name = variant_name; basic; rich; hashes = [] } ->
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
                       |> List.mapi ~f:(fun field_idx c ->
                              let field_val =
                                Fmt.str "%a(%s.t%d)" Identifier.pp
                                  (to_caml_name_for_c c) payload field_idx
                              in
                              store ~dst:"v" ~field_idx ~field_val) )
                   (* Inline record *)
                   | Struct { fields } ->
                     ( fields |> List.length,
                       fields
                       |> List.mapi ~f:(fun field_idx (field_name, c) ->
                              let field_val =
                                Fmt.str "%a(%s.%a)" Identifier.pp
                                  (to_caml_name_for_c c) payload Identifier.pp
                                  field_name
                              in
                              store ~dst:"v" ~field_idx ~field_val) )
                   | _ ->
                     ( 1,
                       [
                         (let field_val =
                            Fmt.str "%a(%s)" Identifier.pp
                              (to_caml_name_for_c constr_type)
                              payload
                          in
                          store ~dst:"v" ~field_idx:0 ~field_val);
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
                 ( case_name (Identifier.to_string variant_name) constr_name,
                   Some contents ))
          |> cases ~indent
        in
        let switch = braced ~indent "switch (x.tag)" cases in
        const pp_statements [ p0; "CAMLlocal1(v)" ]
        ++ cut ++ switch ++ cut
        ++ const pp_statements [ return_val "v" ]
      in
      ("x", `Plain, pp)
    | Variant { basic; rich; hashes; _ } ->
      let basic_only = List.is_empty rich in
      let get_hash = List.Assoc.find_exn hashes ~equal:Identifier.equal in
      let case_name id =
        let tag_suffix = if basic_only then "" else "_tag" in
        Fmt.str "%a_%a%s" Identifier.pp name Identifier.pp id tag_suffix
      in
      let cases =
        let basic =
          basic
          |> List.map ~f:(fun id ->
                 let hash = get_hash id in
                 let case = case_name id in
                 let contents =
                   const pp_statements
                     [ Fmt.str "v = Val_long(%d)" hash; "break" ]
                 in
                 (case, Some contents))
        in
        let rich =
          rich
          |> List.map ~f:(fun (id, constr_c) ->
                 let hash = get_hash id in
                 let case = case_name id in
                 let c_pl = Fmt.str "x.payload->%a" Identifier.pp id in
                 let payload_val =
                   Fmt.str "%a(%s)" Identifier.pp
                     (to_caml_name_for_c constr_c)
                     c_pl
                 in
                 let contents =
                   const pp_statements
                     [
                       Fmt.str "v = caml_alloc_tuple(2)";
                       store ~dst:"v" ~field_idx:0
                         ~field_val:(val_long (Int.to_string hash));
                       store ~dst:"v" ~field_idx:1 ~field_val:payload_val;
                       "break";
                     ]
                 in
                 (case, Some contents))
        in
        cases ~indent (basic @ rich)
      in
      let switch =
        let tag = if basic_only then "x" else "x.tag" in
        braced ~indent (Fmt.str "switch (%s)" tag) cases
      in
      let pp =
        const pp_statements [ p0; l1 "v" ]
        ++ cut ++ switch ++ cut
        ++ const pp_statements [ return_val "v" ]
      in
      ("x", `Plain, pp)
    | Tuple members ->
      let pp =
        const pp_statements
        @@ [
             p0;
             "CAMLlocal1(v)";
             Fmt.str "v = caml_alloc_tuple(%d)" (List.length members);
           ]
        @ (members
          |> List.mapi ~f:(fun field_idx c ->
                 let conv = to_caml_name_for_c c in
                 let field_val =
                   Fmt.str "%a(x.t%d)" Identifier.pp conv field_idx
                 in
                 store ~dst:"v" ~field_idx ~field_val))
        @ [ return_val "v" ]
      in
      ("x", `Plain, pp)
    | Array elem_type ->
      let loop =
        braced ~indent "for (int64_t i = arr.len - 1; i >= 0; i--)"
          (const pp_statements
             [
               "cons = caml_alloc(2, 0)";
               "typeof(*arr.data) x = arr.data[i]";
               store ~dst:"cons" ~field_idx:0
                 ~field_val:
                   (Fmt.str "%a(x)" Identifier.pp
                      (to_caml_name_for_c elem_type));
               store ~dst:"cons" ~field_idx:1 ~field_val:"cli";
               "cli = cons";
             ])
      in
      let pp =
        const pp_statements
          [ p0; "CAMLlocal2(cli, cons)"; "cli = Val_emptylist" ]
        ++ cut ++ loop ++ cut
        ++ const pp_statements [ return_val "cli" ]
      in

      ("arr", `Plain, pp)
    | (Atom _ | String) as c ->
      let pp =
        const pp_statements
          [
            p0;
            "CAMLlocal1(v)";
            Fmt.str "v = %a(x)" Identifier.pp (to_caml_name_for_c c);
            return_val "v";
          ]
      in
      ("x", `Plain, pp)
    | Void ->
      failwith "Void may not be instantiated and is unsupported in to_caml"
  in
  let argtype =
    match kind with `Pointer -> ptr_type | `Plain -> Identifier.to_string name
  in
  let decl =
    signature ~return_type:"value" ~fname:(name |> to_caml_name_for) ~argtype
      ~argname
  in
  { signature = decl; impl = body }

(* TODO: generate *_free() functions *)

let converters name ast =
  (pp_of_caml_converter (name, ast), pp_to_caml_converter (name, ast))

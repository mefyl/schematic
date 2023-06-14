open Base
open Fmt
open C_intf
open C_statements
open C_format

let struct_of_caml name (`Struct { fields }) =
  let open Fmt in
  let open C_statements in
  let open C_format in
  let field_stores =
    fields
    |> List.mapi ~f:(fun field_idx (field_name, field_type) ->
           let conv_fn = of_caml_name_for_c field_type in
           Fmt.str "res.%a = %a(%s)" Identifier.pp field_name Identifier.pp
             conv_fn (field "v" field_idx))
  in
  let statements =
    Fmt.str "%s res" (Identifier.to_string name)
    :: assert_block_size "v" (List.length fields)
    @ field_stores
  in
  ([], const pp_statements statements)

let payload_variant_of_caml
    (`Variant ({ basic; rich; name; _ } : regular_variant)) =
  let plain_case =
    const pp_statements [ "res.tag = Int_val(v)"; "assert(res.tag < n_basic)" ]
  in
  let case : (int * regular_variant_constr) Fmt.t =
   fun fmt (i, { name = field_name; type_ = c; inline }) ->
    let accessor_stmts =
      match c with
      (* Inline tuple payload *)
      | `Atom (_, Some (`Tuple members)) when inline ->
        members
        |> List.mapi ~f:(fun field_idx c ->
               Fmt.str "res.payload->%a.t%d = %a(Field(v, %d))" Identifier.pp
                 field_name field_idx Identifier.pp (of_caml_name_for_c c)
                 field_idx)
      (* Inline record payload *)
      | `Struct { fields } when inline ->
        fields
        |> List.mapi ~f:(fun i (inline_field_name, field_c) ->
               Fmt.str "res.payload->%a.%a = %a(%s)" Identifier.pp field_name
                 Identifier.pp inline_field_name Identifier.pp
                 (of_caml_name_for_c field_c)
                 (field "v" i))
      (* Unary payload *)
      | _ ->
        assert (not inline);
        [
          Fmt.str "res.payload->%a = %a(%s)" Identifier.pp field_name
            Identifier.pp (of_caml_name_for_c c) (field "v" 0);
        ]
    in
    Logs.debug (fun m -> m "Case type %a\n" pp_c c);
    case ~indent (Int.to_string i)
      (Some
         (const pp_statements
            (assert_block_size "v" (List.length accessor_stmts)
            @ accessor_stmts @ [ "break" ])))
      fmt ()
  in
  let rich_case =
    let x = rich |> List.mapi ~f:(fun i x -> (i, x)) in
    let switch =
      vbox ~indent
        (const string "switch (Tag_val(v)) {"
        ++ cut
        ++ const (list ~sep:cut case) x)
      ++ cut ++ const char '}'
    in
    const pp_statements
      [
        "assert(Tag_val(v) < n_rich)";
        "res.tag = n_basic + Tag_val(v)";
        "res.payload = malloc(sizeof(*res.payload))";
      ]
    ++ cut ++ switch
  in
  let open Fmt in
  ( [],
    vbox
      (const pp_statements
         [
           Fmt.str "%a res = {.tag = -1, .payload = NULL}" Identifier.pp name;
           Fmt.str "const int n_basic = %d" (List.length basic);
           Fmt.str "const int n_rich = %d" (List.length rich);
         ]
      ++ cut
      ++ vbox ~indent (const string "if (Is_long(v)) {" ++ cut ++ plain_case)
      ++ cut
      ++ vbox ~indent (const string "} else {" ++ cut ++ rich_case)
      ++ cut ++ const char '}') )

let plain_variant_of_caml name (`Variant ({ basic; _ } : regular_variant)) =
  let pp =
    const pp_statements
      [
        Fmt.str "%a res = Int_val(v)" Identifier.pp name;
        Fmt.str "assert(res < %d)" (List.length basic);
      ]
  in
  ([], pp)

let poly_variant_of_caml name
    (`Poly_variant ({ basic; rich; _ } : poly_variant)) =
  let open Fmt in
  let open C_format in
  let open C_statements in
  let basic_only = List.is_empty rich in
  let case_name = case_name ~rich:(not basic_only) in
  let elses =
    let basic =
      const (list ~sep:cut string)
        (basic
        |> List.map ~f:(fun (id, hash) ->
               let tag = if basic_only then "res" else "res.tag" in
               Fmt.str "else if (v == Val_long(%d)) %s = %a;" hash tag
                 Identifier.pp (case_name name id)))
    in
    if basic_only then basic
    else
      let elses =
        let pp_rich : poly_variant_constr Fmt.t =
         fun fmt { name = id; type_ = constr_c; hash; inline = _ } ->
          (* **NOTE** do no implement inline-related flattening, refer to:
           * https://v2.ocaml.org/manual/intfc.html#ss:c-polyvar *)
          let elseif = Fmt.str "else if (tag == Val_long(%d))" hash in
          let body =
            const pp_statements
              [
                Fmt.str "res.tag = %a" Identifier.pp (case_name name id);
                Fmt.str "res.payload->%a = %a(Field(v, 1))" Identifier.pp id
                  Identifier.pp
                  (of_caml_name_for_c constr_c);
              ]
          in
          (block ~indent elseif body) fmt ()
        in
        const (list ~sep:cut pp_rich) rich
      in

      let body =
        const pp_statements
          (assert_block_size "v" 2
          @ [
              "int tag = Field(v, 0)";
              "res.payload = malloc(sizeof(*res.payload))";
            ])
        ++ cut
        ++ (const string "if (0) {}" ++ cut ++ elses ++ cut
           ++ const string "else assert(0);")
      in
      let rich = block ~indent "else if (Is_block(v))" body in
      basic ++ cut ++ rich
  in
  let if_else_chain =
    const string "if (0) {}" ++ cut ++ elses ++ cut
    ++ const string "else assert(0);"
  in
  let init = if basic_only then "-1" else "{.tag = -1, .payload = NULL}" in
  let pp =
    const pp_statements [ Fmt.str "%a res = %s" Identifier.pp name init ]
    ++ cut ++ if_else_chain
  in
  ([], pp)

let tuple_of_caml (`Tuple members) =
  let tuptype = tuple_name members in
  let pp =
    const pp_statements
      (Fmt.str "%a res" Identifier.pp tuptype
       :: assert_block_size "v" (List.length members)
      @ (members
        |> List.mapi ~f:(fun field_idx c ->
               Fmt.str "res.t%d = %a(%s)" field_idx Identifier.pp
                 (of_caml_name_for_c c) (field "v" field_idx))))
  in
  ([], pp)

let array_of_caml (`Array elem_type) =
  let length_while =
    block ~indent "while (l != Val_emptylist)"
      (const pp_statements [ Fmt.str "l = %s" (field "l" 1); "res.len++" ])
  in
  let set_for =
    block ~indent "for (int64_t i = 0; i < res.len; i++)"
      (const pp_statements @@ assert_block_size "l" 2
      @ [
          Fmt.str "res.data[i] = %a(%s)" Identifier.pp
            (of_caml_name_for_c elem_type)
            (field "l" 0);
          Fmt.str "l = %s" (field "l" 1);
        ])
  in
  let pp =
    const pp_statements
      [
        Fmt.str "%a res = {.len = 0, .data = NULL}" Identifier.pp
          (array_name elem_type);
        "l = v";
      ]
    ++ cut ++ length_while ++ cut
    ++ const pp_statements
         [ "l = v"; "res.data = malloc(sizeof(*res.data) * res.len)" ]
    ++ cut ++ set_for
  in
  ([ "l" ], pp)

let other_of_caml c =
  let pp =
    const pp_statements
      [
        Fmt.str "%a res = %a(v)" C_typeexpr.pp c Identifier.pp
          (of_caml_name_for_c c);
      ]
  in
  ([], pp)

let mapped_of_caml name (`Mapped front_facing) =
  (* TODO: factor lazy named value retrieval *)
  let if_encode =
    block ~indent "if (encode == NULL)"
      (const pp_statements
         [
           Fmt.str {|encode = caml_named_value("%s")|}
             (encode_name_for (name |> Identifier.to_string) front_facing);
         ])
  in
  let pp =
    const pp_statements [ {|static const value* encode = NULL|} ]
    ++ cut ++ if_encode ++ cut
    ++ const pp_statements
         [
           "assert(encode != NULL)";
           "v_back = caml_callback(*encode, v)";
           Fmt.str "%a res = %a(v_back)" Identifier.pp name Identifier.pp
             (of_caml_name_for_c front_facing);
         ]
  in
  ([ "v_back" ], pp)

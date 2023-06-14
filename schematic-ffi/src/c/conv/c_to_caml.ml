open Base
open Fmt
open C_intf
open C_statements
open C_format

let struct_to_caml (`Struct { fields }) =
  let tag = 0 in
  let n_fields = List.length fields in
  let field_stores =
    fields
    |> List.mapi ~f:(fun field_idx (field_name, field_type) ->
           let conv_fn = to_caml_name_for_c field_type in
           let field_val =
             Fmt.str "%a(p.%a)" Identifier.pp conv_fn Identifier.pp field_name
           in
           store ~dst:"v" ~field_idx ~field_val)
  in
  let statements =
    [
      p0;
      l1 "v";
      Fmt.str "int n_fields = %d" n_fields;
      Fmt.str "int tag = %d" tag;
      "v = caml_alloc(n_fields, tag)";
    ]
    @ field_stores
    @ [ return_val "v" ]
  in
  ("p", const pp_statements statements)

let plain_variant_to_caml (`Variant ({ basic; rich; _ } : regular_variant)) =
  assert (List.is_empty rich);
  let pp =
    const pp_statements
      [ Fmt.str "assert(x < %d)" (List.length basic); "return Val_int(x)" ]
  in
  ("x", pp)

let payload_variant_to_caml
    (`Variant ({ name = variant_name; basic; rich } : regular_variant)) =
  let pp =
    let basic_cases : (string * unit Fmt.t) list =
      basic
      |> List.mapi ~f:(fun i constr_name ->
             ( Identifier.to_string constr_name,
               const pp_statements [ Fmt.str "v = Val_int(%d)" i; "break" ] ))
    in
    let rich_cases : (string * unit Fmt.t) list =
      rich
      |> List.mapi
           ~f:(fun
                i
                ({ name = constr_name; type_ = constr_type; inline } :
                  regular_variant_constr)
              ->
             Logs.debug (fun m ->
                 m "Rich case constructor type: %a" pp_c constr_type);
             let payload = Fmt.str "x.payload->%a" Identifier.pp constr_name in
             let n_fields, stmts =
               match constr_type with
               (* Inline tuple *)
               | `Atom (_, Some (`Tuple members)) when inline ->
                 ( members |> List.length,
                   members
                   |> List.mapi ~f:(fun field_idx c ->
                          let field_val =
                            Fmt.str "%a(%s.t%d)" Identifier.pp
                              (to_caml_name_for_c c) payload field_idx
                          in
                          store ~dst:"v" ~field_idx ~field_val) )
               (* Inline record *)
               | `Struct { fields } when inline ->
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
                 assert (not inline);
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
             let case_name =
               case_name ~rich:true variant_name (Identifier.make constr_name)
               |> Identifier.to_string
             in
             (case_name, Some contents))
      |> cases ~indent
    in
    let switch = block ~indent "switch (x.tag)" cases in
    const pp_statements [ p0; l1 "v" ]
    ++ cut ++ switch ++ cut
    ++ const pp_statements [ return_val "v" ]
  in
  ("x", pp)

let poly_variant_to_caml (`Poly_variant { name; basic; rich; _ }) =
  (* **NOTE** do no implement inline-related flattening, refer to:
   * https://v2.ocaml.org/manual/intfc.html#ss:c-polyvar *)
  let basic_only = List.is_empty rich in
  let case_name = case_name ~rich:(not basic_only) in
  let cases =
    let basic =
      basic
      |> List.map ~f:(fun (id, hash) ->
             let case = case_name name id |> Identifier.to_string in
             let contents =
               const pp_statements [ Fmt.str "v = Val_long(%d)" hash; "break" ]
             in
             (case, Some contents))
    in
    let rich =
      rich
      |> List.map ~f:(fun { name = id; type_ = constr_c; hash; inline = _ } ->
             let case = case_name name id |> Identifier.to_string in
             let c_pl = Fmt.str "x.payload->%a" Identifier.pp id in
             let payload_val =
               Fmt.str "%a(%s)" Identifier.pp (to_caml_name_for_c constr_c) c_pl
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
    block ~indent (Fmt.str "switch (%s)" tag) cases
  in
  let pp =
    const pp_statements [ p0; l1 "v" ]
    ++ cut ++ switch ++ cut
    ++ const pp_statements [ return_val "v" ]
  in
  ("x", pp)

let tuple_to_caml (`Tuple members) =
  let pp =
    const pp_statements
    @@ [ p0; l1 "v"; Fmt.str "v = caml_alloc_tuple(%d)" (List.length members) ]
    @ (members
      |> List.mapi ~f:(fun field_idx c ->
             let conv = to_caml_name_for_c c in
             let field_val = Fmt.str "%a(x.t%d)" Identifier.pp conv field_idx in
             store ~dst:"v" ~field_idx ~field_val))
    @ [ return_val "v" ]
  in
  ("x", pp)

let array_to_caml (`Array elem_type) =
  let loop =
    block ~indent "for (int64_t i = arr.len - 1; i >= 0; i--)"
      (const pp_statements
         [
           "cons = caml_alloc(2, 0)";
           "typeof(*arr.data) x = arr.data[i]";
           store ~dst:"cons" ~field_idx:0
             ~field_val:
               (Fmt.str "%a(x)" Identifier.pp (to_caml_name_for_c elem_type));
           store ~dst:"cons" ~field_idx:1 ~field_val:"cli";
           "cli = cons";
         ])
  in
  let pp =
    const pp_statements [ p0; l2 "cli" "cons"; "cli = Val_emptylist" ]
    ++ cut ++ loop ++ cut
    ++ const pp_statements [ return_val "cli" ]
  in
  ("arr", pp)

let mapped_to_caml name (`Mapped front_facing) =
  let if_decode =
    block ~indent "if (decode == NULL)"
      (const pp_statements
         [
           Fmt.str {|decode = caml_named_value("%s")|}
             (decode_name_for (name |> Identifier.to_string) front_facing);
         ])
  in
  let pp =
    const pp_statements
      [ p0; l2 "v_front" "v"; {|static const value* decode = NULL|} ]
    ++ cut ++ if_decode ++ cut
    ++ const pp_statements
         [
           "assert(decode != NULL)";
           Fmt.str {|v_front = %a(x)|} Identifier.pp
             (to_caml_name_for_c front_facing);
           {|v = caml_callback(*decode, v_front)|};
           {|CAMLreturn(v)|};
         ]
  in
  ("x", pp)

let other_to_caml c =
  let pp =
    const pp_statements
      [
        p0;
        l1 "v";
        Fmt.str "v = %a(x)" Identifier.pp (to_caml_name_for_c c);
        return_val "v";
      ]
  in
  ("x", pp)

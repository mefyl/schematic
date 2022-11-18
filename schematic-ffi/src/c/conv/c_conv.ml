open Base
open C_intf
open C_format
open C_statements

let safety_checks_for name ~fname ~argname =
  let open Fmt in
  let checking, checked = checking_checked_for name in
  let main_if =
    let if_cond = Fmt.str "if (!(%s || %s))" checking checked in
    let body =
      let if_inner =
        block ~indent
          (Fmt.str "if (%s == NULL)" C_builtins.poly_compare_symbol)
          (const pp_statements
             [
               Fmt.str {|%s = caml_named_value("%s")|}
                 C_builtins.poly_compare_symbol Runtime.poly_cmp_callback_name;
             ])
      in
      const pp_statements [ Fmt.str "%s = true" checking ]
      ++ cut ++ if_inner ++ cut
      ++ const pp_statements
           [
             Fmt.str {|printf("checking type %a\n")|} Identifier.pp name;
             Fmt.str "assert(Bool_val(caml_callback2(*%s, %a(%a(%s)), %s)))"
               C_builtins.poly_compare_symbol Identifier.pp
               (name |> to_caml_name_for) Identifier.pp fname argname argname;
             Fmt.str "%s = true" checked;
             Fmt.str "%s = false" checking;
           ]
    in
    block ~indent if_cond body
  in
  main_if ++ cut

let pp_of_caml_converter ?(safety_checks = false) ((name : Identifier.t), ast) =
  let open C_of_caml in
  let open Fmt in
  let caml_locals, (body : unit Fmt.t) =
    match ast with
    | `Void ->
      failwith
        "`Void may not be instantiated, and therefore cannot be converted"
    | `Option c -> payload_variant_of_caml (reified_option_c c)
    | `Struct _ as s -> struct_of_caml name s
    (* Plain-only regular variant *)
    | `Variant ({ rich = []; _ } : regular_variant) as v ->
      plain_variant_of_caml name v (* Regular variant with payloads *)
    | `Variant (_ : regular_variant) as v -> payload_variant_of_caml v
    (* Polymorphic variant *)
    | `Poly_variant (_ : poly_variant) as v -> poly_variant_of_caml name v
    | `Tuple _ as t -> tuple_of_caml t
    | `Array _ as a -> array_of_caml a
    (* The considered type is merely an alias for another *)
    | (`Atom _ | #c_leaf) as c -> other_of_caml c
    | `Mapped _ as m -> mapped_of_caml name m
  in
  let return_type = Identifier.to_string name in
  let fname = name |> of_caml_name_for in
  let argname = "v" in
  let body =
    let safety_checks =
      if safety_checks then safety_checks_for name ~fname ~argname else Fmt.nop
    in
    let caml_params = const pp_statements [ p1 "v" ] ++ cut in
    let caml_locals =
      match caml_locals with
      | [] -> nop
      | [ x ] -> const pp_statements [ l1 x ] ++ cut
      | _ -> failwith "CAMLlocalN with n > 1 unsupported yet"
    in
    caml_params ++ caml_locals ++ const safety_checks () ++ body ++ cut
    ++ const pp_statements [ return_t return_type "res" ]
  in
  let decl = signature ~return_type ~fname ~argtype:"value" ~argname in
  { signature = decl; impl = body }

let pp_to_caml_converter ((name : Identifier.t), (ast : c)) =
  let open Fmt in
  let open C_to_caml in
  let argname, (body : unit Fmt.t) =
    match ast with
    | `Void ->
      failwith "`Void may not be instantiated and is unsupported in to_caml"
    | `Option c -> payload_variant_to_caml (reified_option_c c)
    | `Struct _ as s -> struct_to_caml s
    (* Plain-only regular variant *)
    | `Variant ({ rich = []; _ } : regular_variant) as v ->
      plain_variant_to_caml v
    (* Regular variant with payloads *)
    | `Variant (_ : regular_variant) as v -> payload_variant_to_caml v
    | `Poly_variant (_ : poly_variant) as v -> poly_variant_to_caml v
    | `Tuple _ as t -> tuple_to_caml t
    | `Array _ as a -> array_to_caml a
    | `Mapped _ as m -> mapped_to_caml name m
    | (`Atom _ | #c_leaf) as c -> other_to_caml c
  in
  let argtype = Identifier.to_string name in
  let decl =
    signature ~return_type:"value" ~fname:(name |> to_caml_name_for) ~argtype
      ~argname
  in
  { signature = decl; impl = body }

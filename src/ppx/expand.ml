open Base
open Poly
open Ppxlib

let check_unknown_attribute ~loc name =
  if String.is_prefix ~prefix:"schematic." name then
    Ppxlib.Location.raise_errorf ~loc
      "schematic-ppx: unknown schematic annotation %s" name

module List = struct
  include List

  let fold_righti ~f ~init =
    let rec fold i = function
      | [] -> init
      | hd :: tl ->
        let tl = fold (i + 1) tl in
        f i tl hd
    in
    fold 0
end

let pattern_id ~loc txt =
  {
    ppat_desc = Ppat_var { txt; loc };
    ppat_loc = loc;
    ppat_loc_stack = [];
    ppat_attributes = [];
  }

let pattern_string ~loc txt =
  {
    ppat_desc = Ppat_constant (Pconst_string (txt, None));
    ppat_loc = loc;
    ppat_loc_stack = [];
    ppat_attributes = [];
  }

let expr_id ~loc txt =
  {
    pexp_desc = Pexp_ident { txt; loc };
    pexp_loc = loc;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

let expr_apply ~loc f args =
  {
    pexp_desc = Pexp_apply (f, args);
    pexp_loc = loc;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

let expr_list ~loc list =
  let f m l = [%expr [%e m] :: [%e l]]
  and init = [%expr []] in
  List.fold_right ~f ~init list

let expr_array ~loc list =
  {
    pexp_desc = Pexp_array list;
    pexp_loc = loc;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

let expr_bool ~loc b =
  if b then
    [%expr true]
  else
    [%expr false]

let expr_int ~loc i =
  {
    pexp_desc = Pexp_constant (Pconst_integer (Int.to_string i, None));
    pexp_loc = loc;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

let expect_string ~loc = function
  | Pexp_constant (Pconst_string (value, None)) -> value
  | _ -> Ppxlib.Location.raise_errorf ~loc "schematic-ppx: string expected"

let expect_string_constant ~loc = function
  | PStr
      [%str [%e? { pexp_desc = Pexp_constant (Pconst_string (txt, None)); _ }]]
    ->
    txt
  | _ ->
    Ppxlib.Location.raise_errorf ~loc "schematic-ppx: string constant expected"

let expect_int ~loc = function
  | Pexp_constant (Pconst_integer (value, None)) -> Int.of_string value
  | _ -> Ppxlib.Location.raise_errorf ~loc "schematic-ppx: int expected"

let rename name =
  let len = String.length name in
  if name.[len - 1] = '_' then
    String.sub name ~pos:0 ~len:(len - 1)
  else
    name

let schema_name s =
  let s = rename s in
  if s = "t" then
    "schema"
  else
    s ^ "_schema"

let schema_id = function
  | Lident name -> Lident (schema_name name)
  | Ldot (q, name) -> Ldot (q, schema_name name)
  | Lapply (_, _) -> failwith "Lapply"

let expr_string ~loc txt =
  {
    pexp_desc = Pexp_constant (Pconst_string (txt, None));
    pexp_loc = loc;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

let expr_option ~loc expr = function
  | None -> [%expr None]
  | Some e -> [%expr Some [%e expr ~loc e]]

let rec encode_type ~loc = function
  (* Mapping *)
  | [%type: (string * [%t? t]) list] ->
    [%expr { id = None; descriptor = Mapping [%e encode_type ~loc t] }]
  (* Plain type *)
  | { ptyp_desc = Ptyp_constr ({ txt; loc }, []); _ } ->
    expr_id ~loc (schema_id txt)
  (* Parametric type *)
  | { ptyp_desc = Ptyp_constr ({ txt; loc }, args); _ } -> (
    match (txt, args) with
    | Lident "list", [ arg ] ->
      [%expr { id = None; descriptor = List [%e encode_type ~loc arg] }]
    | _ ->
      let schema = expr_id ~loc (schema_id txt)
      and f t = (Nolabel, encode_type ~loc t) in
      {
        pexp_desc = Pexp_apply (schema, List.map ~f args);
        pexp_loc = loc;
        pexp_loc_stack = [];
        pexp_attributes = [];
      } )
  (* Tuple *)
  | { ptyp_desc = Ptyp_tuple types; _ } ->
    let field_name i { ptyp_loc = loc; _ } = (loc, "e" ^ Int.to_string i)
    and constructed =
      let f i _ = expr_id ~loc (Lident ("e" ^ Int.to_string i)) in
      {
        pexp_desc = Pexp_tuple (List.mapi ~f types);
        pexp_loc = loc;
        pexp_loc_stack = [];
        pexp_attributes = [];
      }
    and deconstructed =
      let f i _ = pattern_id ~loc ("e" ^ Int.to_string i) in
      {
        ppat_desc = Ppat_tuple (List.mapi ~f types);
        ppat_loc = loc;
        ppat_loc_stack = [];
        ppat_attributes = [];
      }
    in
    let body =
      combinator ~loc types field_name constructed deconstructed
        (tuple_schema ~loc types)
    in
    [%expr { id = None; descriptor = [%e body] }]
  (* Type variable *)
  | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ } ->
    [%expr [%e expr_id ~loc (schema_id (Lident name))]]
  (* Unsupported *)
  | core_type ->
    Ppxlib.Location.raise_errorf ~loc "schematic-ppx: non supported type %a"
      Pprintast.core_type core_type

and tuple_schema ~loc types =
  let args = List.map ~f:(encode_type ~loc) types in
  let init = [%expr End] in
  let f r l = [%expr Tuple { schema = [%e r]; rest = [%e l] }] in
  List.fold_right ~f ~init args

and combinator :
    type f.
    loc:location ->
    f list ->
    (int -> f -> location * label) ->
    expression ->
    pattern ->
    expression ->
    expression =
 fun ~loc fields field_name constructed deconstructed obj ->
  let construct =
    let pattern =
      let pattern i f =
        let loc, name = field_name i f in
        pattern_id ~loc name
      in
      let f i tl field =
        let pattern = pattern i field in
        [%pat? [%p pattern], [%p tl]]
      in
      List.fold_righti ~f ~init:[%pat? ()] fields
    in
    {
      pexp_desc = Pexp_fun (Nolabel, None, pattern, constructed);
      pexp_loc = loc;
      pexp_loc_stack = [];
      pexp_attributes = [];
    }
  and deconstruct =
    let value =
      let expr i f =
        let loc, name = field_name i f in
        expr_id ~loc (Lident name)
      in
      let f i tl field =
        let expr = expr i field in
        [%expr [%e expr], [%e tl]]
      in
      List.fold_righti ~f ~init:[%expr ()] fields
    in
    {
      pexp_desc = Pexp_fun (Nolabel, None, deconstructed, value);
      pexp_loc = loc;
      pexp_loc_stack = [];
      pexp_attributes = [];
    }
  in
  [%expr Map ([%e construct], [%e deconstruct], [%e obj])]

type specs = {
  default : expression option;
  description : string option;
  examples : expression list;
  maximum : int option;
  minimum : int option;
}

let specs_empty =
  {
    default = None;
    description = None;
    examples = [];
    maximum = None;
    minimum = None;
  }

let expand_type ~loc t =
  let expand_record fields =
    let schema =
      let expand_fields fields =
        let expand_field = function
          | {
              pld_attributes;
              pld_loc = loc;
              pld_name = { txt; _ };
              pld_type;
              _;
            } ->
            let read_attribute
                (txt, inline, specs) { attr_name; attr_loc = loc; attr_payload }
                =
              match attr_name.txt with
              | "schematic.properties" -> (
                match attr_payload with
                | PStr
                    [%str
                      [%e?
                        {
                          pexp_desc = Pexp_record (properties, None);
                          pexp_loc = loc;
                          _;
                        }]] ->
                  let prop specs = function
                    | ( { txt = Lident name; _ },
                        ({ pexp_desc = value; pexp_loc = loc; _ } as desc) )
                      -> (
                      match name with
                      | "description" ->
                        {
                          specs with
                          description = Some (expect_string ~loc value);
                        }
                      | "default" -> { specs with default = Some desc }
                      | "example" -> { specs with examples = [ desc ] }
                      | "minimum" ->
                        { specs with maximum = Some (expect_int ~loc value) }
                      | "maximum" ->
                        { specs with minimum = Some (expect_int ~loc value) }
                      | "unique" -> specs
                      | _ ->
                        Ppxlib.Location.raise_errorf ~loc
                          "schematic-ppx: unrecognized property %S" name )
                    | _ ->
                      Ppxlib.Location.raise_errorf ~loc
                        "schematic-ppx: unrecognized property"
                  in
                  (txt, inline, List.fold ~init:specs_empty ~f:prop properties)
                | _ ->
                  Ppxlib.Location.raise_errorf ~loc
                    "schematic-ppx: record payload expected" )
              | "schematic.as" ->
                (expect_string_constant ~loc attr_payload, inline, specs)
              | "schematic.inline" -> (
                match attr_payload with
                | PStr [] ->
                  if not inline then
                    (txt, true, specs)
                  else
                    Ppxlib.Location.raise_errorf ~loc
                      "schematic-ppx: inline specified multiple times"
                | _ ->
                  Ppxlib.Location.raise_errorf ~loc
                    "schematic-ppx: no payload expected" )
              | name ->
                let () = check_unknown_attribute ~loc name in
                (txt, inline, specs)
            in
            let txt, inline, specs =
              List.fold_left ~f:read_attribute ~init:(txt, false, specs_empty)
                pld_attributes
            in
            let requirement, schema =
              match pld_type with
              | {
               ptyp_desc = Ptyp_constr ({ txt = Lident "option"; _ }, [ t ]);
               _;
              } ->
                ([%expr Optional], encode_type ~loc t)
              | t -> (
                match specs.default with
                | Some default ->
                  ([%expr Default [%e default]], encode_type ~loc t)
                | None -> ([%expr Required], encode_type ~loc t) )
            in
            ( requirement,
              expr_string ~loc (rename txt),
              schema,
              expr_bool ~loc inline,
              specs )
        in
        List.map ~f:expand_field fields
      in
      let init = [%expr End] in
      let f (requirement, field, schema, inline, specs) rest =
        [%expr
          Object
            {
              description = [%e expr_option ~loc expr_string specs.description];
              examples = [%e expr_list ~loc specs.examples];
              field = [%e field];
              inline = [%e inline];
              maximum = [%e expr_option ~loc expr_int specs.maximum];
              minimum = [%e expr_option ~loc expr_int specs.minimum];
              requirement = [%e requirement];
              rest = [%e rest];
              schema = [%e schema];
              title = None;
            }]
      in
      List.fold_right ~f ~init (expand_fields fields)
    and field_name _ { pld_name = { txt; loc }; _ } = (loc, txt)
    and constructed =
      let f { pld_name = { txt; loc }; _ } =
        ( { txt = Lident txt; loc },
          {
            pexp_desc = Pexp_ident { txt = Lident txt; loc };
            pexp_loc = loc;
            pexp_loc_stack = [];
            pexp_attributes = [];
          } )
      in
      {
        pexp_desc = Pexp_record (List.map ~f fields, None);
        pexp_loc = loc;
        pexp_loc_stack = [];
        pexp_attributes = [];
      }
    and deconstructed =
      let f { pld_name = { txt; loc }; _ } =
        ( { txt = Lident txt; loc },
          {
            ppat_desc = Ppat_var { txt; loc };
            ppat_loc = loc;
            ppat_loc_stack = [];
            ppat_attributes = [];
          } )
      in
      {
        ppat_desc = Ppat_record (List.map ~f fields, Closed);
        ppat_loc = loc;
        ppat_loc_stack = [];
        ppat_attributes = [];
      }
    in
    combinator ~loc fields field_name constructed deconstructed schema
  and expand_variant options =
    let f
        {
          pcd_name = { txt; loc = _ };
          pcd_args;
          pcd_loc = loc;
          pcd_attributes = attrs;
          _;
        } =
      let schema =
        match pcd_args with
        | Pcstr_tuple [] ->
          let alias =
            let f alias = function
              | {
                  attr_name = { txt = "schematic.as"; _ };
                  attr_loc = loc;
                  attr_payload;
                } ->
                if Option.is_some alias then
                  Ppxlib.Location.raise_errorf ~loc
                    "schematic-ppx: duplicate schematic.as annotation"
                else
                  Some (expect_string_constant ~loc attr_payload)
              | { attr_name = { txt = name; _ }; attr_loc = loc; _ } ->
                let () = check_unknown_attribute ~loc name in
                alias
            in
            List.fold_left ~init:None ~f attrs
          in
          [%expr
            {
              id = None;
              descriptor =
                String_const
                  [%e
                    expr_string ~loc
                      (Option.value ~default:(String.lowercase txt) alias)];
            }]
        | Pcstr_tuple [ field ] -> encode_type ~loc field
        | Pcstr_tuple fields -> tuple_schema ~loc fields
        | Pcstr_record _ -> failwith "not implemented"
      and construct =
        match pcd_args with
        | Pcstr_tuple [] ->
          let pattern =
            {
              ppat_desc = Ppat_construct ({ txt = Lident txt; loc }, None);
              ppat_loc = loc;
              ppat_loc_stack = [];
              ppat_attributes = [];
            }
          in
          [%expr
            function
            | [%p pattern] -> Some ()
            | _ -> None]
        | Pcstr_tuple _ ->
          let pattern =
            {
              ppat_desc =
                Ppat_construct
                  ( { txt = Lident txt; loc },
                    Some
                      {
                        ppat_desc = Ppat_var { txt = "x"; loc };
                        ppat_loc = loc;
                        ppat_loc_stack = [];
                        ppat_attributes = [];
                      } );
              ppat_loc = loc;
              ppat_loc_stack = [];
              ppat_attributes = [];
            }
          in
          [%expr
            function
            | [%p pattern] -> Some x
            | _ -> None]
        | Pcstr_record _ -> failwith "not implemented"
      and destruct =
        match pcd_args with
        | Pcstr_tuple [] ->
          let value =
            {
              pexp_desc = Pexp_construct ({ txt = Lident txt; loc }, None);
              pexp_loc = loc;
              pexp_loc_stack = [];
              pexp_attributes = [];
            }
          in
          [%expr
            function
            | () -> [%e value]]
        | Pcstr_tuple _ ->
          let value =
            {
              pexp_desc =
                Pexp_construct
                  ( { txt = Lident txt; loc },
                    Some
                      {
                        pexp_desc = Pexp_ident { txt = Lident "x"; loc };
                        pexp_loc = loc;
                        pexp_loc_stack = [];
                        pexp_attributes = [];
                      } );
              pexp_loc = loc;
              pexp_loc_stack = [];
              pexp_attributes = [];
            }
          in
          [%expr
            function
            | x -> [%e value]]
        | Pcstr_record _ -> failwith "not implemented"
      in
      [%expr
        Case
          {
            schema = [%e schema];
            encoder = [%e construct];
            decoder = [%e destruct];
          }]
    in
    [%expr Union [%e expr_list ~loc (List.map ~f options)]]
  in
  let body, recursive =
    match t.ptype_kind with
    | Ptype_record fields -> (expand_record fields, true)
    | Ptype_abstract -> (
      match t.ptype_manifest with
      | Some t -> ([%expr [%e encode_type ~loc t].descriptor], false)
      | None -> failwith "no manifest on abstract type" )
    | Ptype_open -> failwith "unsupported Ptype_open"
    | Ptype_variant options -> (expand_variant options, true)
  and wrap arg exp =
    match arg with
    | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ }, Invariant ->
      [%expr fun [%p pattern_id ~loc (schema_name name)] -> [%e exp]]
    | t, _ ->
      Ppxlib.Location.raise_errorf ~loc
        "schematic-ppx: non supported type parameter %a" Pprintast.core_type t
  in
  let body =
    [%expr
      {
        id = Some [%e expr_string ~loc (rename t.ptype_name.txt)];
        descriptor = [%e body];
      }]
  in
  (List.fold_right ~f:wrap ~init:body t.ptype_params, recursive)

let expand ~loc ~path:_ ~inside (_, types) =
  let f t =
    let name = schema_name t.ptype_name.txt in
    let pattern = pattern_id ~loc:t.ptype_name.loc name in
    let body, recursive = expand_type ~loc t in
    let body =
      [%expr
        let _ = unit_schema in
        [%e body]]
    in
    let body =
      if inside then
        [%expr
          let open Schema in
          [%e body]]
      else
        [%expr
          let open Schematic.Schema in
          [%e body]]
    in
    let body =
      if recursive then
        [%expr
          let _ = [%e expr_id ~loc (Lident name)] in
          [%e body]]
      else
        body
    in
    ( recursive,
      { pvb_pat = pattern; pvb_expr = body; pvb_loc = loc; pvb_attributes = [] }
    )
  in
  let schemas = List.map ~f types in
  [
    {
      pstr_desc =
        Pstr_value
          ( ( if List.exists ~f:fst schemas then
              Recursive
            else
              Nonrecursive ),
            List.map ~f:snd schemas );
      pstr_loc = loc;
    };
  ]

(* let rec [%p pattern] = [%str let [%p pattern] = [%e body]] *)

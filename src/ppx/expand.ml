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

let name suffix s =
  let s = rename s in
  if s = "t" then
    suffix
  else
    s ^ "_" ^ suffix

let schema_name = name "schema"

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

module type Expansion = sig
  type closed

  type expanded

  val close :
    loc:location ->
    type_declaration ->
    (core_type * variance) list ->
    expanded ->
    closed

  val combine : loc:location -> closed list -> structure_item_desc

  val expand_abstract_type : loc:location -> core_type -> expanded

  val expand_record : loc:location -> label_declaration list -> expanded

  val expand_variant : loc:location -> constructor_declaration list -> expanded
end

module Schema : Expansion = struct
  type closed = value_binding * bool

  type expanded = expression * bool

  let expand_record ~loc fields =
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
    (combinator ~loc fields field_name constructed deconstructed schema, true)

  let expand_variant ~loc options =
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
    ([%expr Union [%e expr_list ~loc (List.map ~f options)]], true)

  let expand_abstract_type ~loc t =
    ([%expr [%e encode_type ~loc t].descriptor], false)

  let close ~loc t params (body, recursive) =
    let init =
      [%expr
        {
          id = Some [%e expr_string ~loc (rename t.ptype_name.txt)];
          descriptor = [%e body];
        }]
    and wrap arg exp =
      match arg with
      | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ }, Invariant ->
        [%expr fun [%p pattern_id ~loc (schema_name name)] -> [%e exp]]
      | t, _ ->
        Ppxlib.Location.raise_errorf ~loc
          "schematic-ppx: non supported type parameter %a" Pprintast.core_type t
    in
    let body = List.fold_right ~f:wrap ~init params in
    let name = schema_name t.ptype_name.txt in
    let pattern = pattern_id ~loc:t.ptype_name.loc name in
    let body =
      if recursive then
        [%expr
          let _ = [%e expr_id ~loc (Lident name)] in
          [%e body]]
      else
        body
    in
    let body =
      { pvb_pat = pattern; pvb_expr = body; pvb_loc = loc; pvb_attributes = [] }
    in
    (body, recursive)

  let combine ~loc:_ expanded =
    Pstr_value
      ( ( if List.exists ~f:snd expanded then
          Recursive
        else
          Nonrecursive ),
        List.map ~f:fst expanded )
end

module Jsoo_type : Expansion = struct
  type closed = type_declaration

  type expanded = core_type

  let expand_ident = function
    | Lident n -> Lident (name "js" n)
    | Ldot (ident, m) -> Ldot (ident, name "js" m)
    | Lapply _ -> failwith "Lapply"

  let rec expand_type t =
    match t.ptyp_desc with
    | Ptyp_constr (name, args) ->
      {
        t with
        ptyp_desc =
          Ptyp_constr
            ( { name with txt = expand_ident name.txt },
              List.map ~f:expand_type args );
      }
    | _ ->
      Ppxlib.Location.raise_errorf ~loc:t.ptyp_loc
        "schematic-ppx: unsupported type"

  let expand_record ~loc fields =
    let f { pld_name; pld_type; pld_loc; _ } : object_field =
      let t = expand_type pld_type in
      let t = [%type: [%t t] Js.readonly_prop] in
      {
        pof_desc = Otag ({ pld_name with txt = rename pld_name.txt }, t);
        pof_loc = pld_loc;
        pof_attributes = [];
      }
    in
    {
      ptyp_desc =
        Ptyp_constr
          ( { loc; txt = Ldot (Ldot (Lident "Js_of_ocaml", "Js"), "t") },
            [
              {
                ptyp_desc = Ptyp_object (List.map ~f fields, Closed);
                ptyp_attributes = [];
                ptyp_loc = loc;
                ptyp_loc_stack = [];
              };
            ] );
      ptyp_attributes = [];
      ptyp_loc = loc;
      ptyp_loc_stack = [];
    }

  (* [%type: < i : int Js.readonly_prop > Js_of_ocaml.Js.t] *)

  let expand_variant ~loc _options =
    Ppxlib.Location.raise_errorf ~loc "schematic-ppx: variant are unsupported"

  let expand_abstract_type ~loc:_ t = expand_type t

  let close ~loc { ptype_name = { loc = name_loc; txt }; _ } params js =
    {
      ptype_name = { loc = name_loc; txt = name "js" txt };
      ptype_params = params;
      ptype_cstrs = [];
      ptype_kind = Ptype_abstract;
      ptype_private = Public;
      ptype_manifest = Some js;
      ptype_loc = loc;
      ptype_attributes = [];
    }

  let combine ~loc:_ items = Pstr_type (Recursive, items)
end

module Expander (Expansion : Expansion) = struct
  let expand ~loc t =
    let f t =
      let body =
        match t.ptype_kind with
        | Ptype_record fields -> Expansion.expand_record ~loc fields
        | Ptype_abstract -> (
          match t.ptype_manifest with
          | Some t -> Expansion.expand_abstract_type ~loc t
          | None -> failwith "no manifest on abstract type" )
        | Ptype_open -> failwith "unsupported Ptype_open"
        | Ptype_variant options -> Expansion.expand_variant ~loc options
      in
      Expansion.close ~loc t t.ptype_params body
    in
    let expanded = List.map ~f t in
    Expansion.combine ~loc expanded
end

module Schema_expander = Expander (Schema)
module Jsoo_type_expander = Expander (Jsoo_type)

let expand ~loc ~path:_ ~inside (_, types) jsoo =
  let contents =
    let jsoo =
      if jsoo then
        [ { pstr_desc = Jsoo_type_expander.expand ~loc types; pstr_loc = loc } ]
      else
        []
    in
    { pstr_desc = Schema_expander.expand ~loc types; pstr_loc = loc } :: jsoo
  and namespace =
    if inside then
      [%str
        open Schema

        let _ = unit_schema]
    else
      [%str
        open Schematic.Schema

        let _ = unit_schema]
  and jsoo =
    if jsoo then
      [%str
        open Schematic_jsoo.Jsoo

        let _ = fun (_ : int_js) -> ()]
    else
      []
  in
  namespace @ jsoo @ contents

let add inside =
  let str_type_decl =
    let open Deriving.Args in
    let arg =
      let jsoo = flag "jsoo" in
      empty +> jsoo
    in
    Deriving.Generator.make ~attributes:[] arg (expand ~inside)
  in
  Deriving.add ~str_type_decl "schema"

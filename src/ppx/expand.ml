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

(* Helpers *)

let pexp ~loc ~desc =
  {
    pexp_desc = desc;
    pexp_loc = loc;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

let ppat ~loc ~desc =
  {
    ppat_desc = desc;
    ppat_loc = loc;
    ppat_loc_stack = [];
    ppat_attributes = [];
  }

let ptyp ~loc ~desc =
  {
    ptyp_desc = desc;
    ptyp_loc = loc;
    ptyp_loc_stack = [];
    ptyp_attributes = [];
  }

module Expr = struct
  let id ~loc name =
    {
      pexp_desc = Pexp_ident { txt = Lident name; loc };
      pexp_loc = loc;
      pexp_loc_stack = [];
      pexp_attributes = [];
    }

  let record ~loc fields =
    let f (name, value) = ({ txt = Lident name; loc }, value) in
    {
      pexp_desc = Pexp_record (List.map ~f fields, None);
      pexp_loc = loc;
      pexp_loc_stack = [];
      pexp_attributes = [];
    }

  let tuple ~loc exps =
    {
      pexp_desc = Pexp_tuple exps;
      pexp_loc = loc;
      pexp_loc_stack = [];
      pexp_attributes = [];
    }

  let apply ~loc f = function
    | [] -> f
    | args ->
      {
        pexp_desc = Pexp_apply (f, List.map ~f:(fun a -> (Nolabel, a)) args);
        pexp_loc = loc;
        pexp_loc_stack = [];
        pexp_attributes = [];
      }
end

module Pattern = struct
  let id ~loc name =
    {
      ppat_desc = Ppat_var { txt = name; loc };
      ppat_loc = loc;
      ppat_loc_stack = [];
      ppat_attributes = [];
    }

  let tuple ~loc patterns =
    {
      ppat_desc = Ppat_tuple patterns;
      ppat_loc = loc;
      ppat_loc_stack = [];
      ppat_attributes = [];
    }
end

module Warning = struct
  let unused_rec ~loc =
    {
      attr_name = { txt = "warning"; loc };
      attr_payload =
        PStr
          [
            {
              pstr_desc =
                Pstr_eval
                  ( {
                      pexp_desc = Pexp_constant (Pconst_string ("-39", None));
                      pexp_loc = loc;
                      pexp_loc_stack = [];
                      pexp_attributes = [];
                    },
                    [] );
              pstr_loc = loc;
            };
          ];
      attr_loc = loc;
    }
end

(* Caml variant constructor of the form [Foo of int * int] surprisingly do not
   have one tuple argument, but two individual int arguments, while [Foo of (int
   * int)] has a single tuple argument. Map the first case to the second and
   back.

   While we're at it, map constructor record arguments to tuple too, to
   streamline constructors arguments handling. *)

let constructor_arguments_to_type ~loc = function
  | Pcstr_tuple [] -> None
  | Pcstr_tuple [ t ] -> Some t
  | Pcstr_tuple types -> Some (ptyp ~loc ~desc:(Ptyp_tuple types))
  | Pcstr_record types ->
    let f { pld_type; _ } = pld_type in
    Some (ptyp ~loc ~desc:(Ptyp_tuple (List.map ~f types)))

let constructor_arguments_to_pattern ~loc prefix = function
  | Pcstr_tuple [] -> None
  | Pcstr_tuple [ _ ] -> Some (Pattern.id ~loc prefix)
  | Pcstr_tuple types ->
    let f i _ = Pattern.id ~loc (prefix ^ Int.to_string i) in
    Some (Pattern.tuple ~loc (List.mapi ~f types))
  | Pcstr_record types ->
    let f i _ = Pattern.id ~loc (prefix ^ Int.to_string i) in
    Some (Pattern.tuple ~loc (List.mapi ~f types))

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

let suffix_id ~loc suffix = function
  | Lident n -> Lident (name suffix n)
  | Ldot (q, n) -> Ldot (q, name suffix n)
  | Lapply (_, _) ->
    Ppxlib.Location.raise_errorf ~loc "Functor application found as a type"

let suffix_id_loc suffix ({ txt; loc } as id) =
  { id with txt = suffix_id ~loc suffix txt }

let schema_id ~loc = suffix_id ~loc "schema"

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
    expr_id ~loc (schema_id ~loc txt)
  (* Parametric type *)
  | { ptyp_desc = Ptyp_constr ({ txt; loc }, args); _ } -> (
    match (txt, args) with
    | Lident "list", [ arg ] ->
      [%expr { id = None; descriptor = List [%e encode_type ~loc arg] }]
    | _ ->
      let schema = expr_id ~loc (schema_id ~loc txt)
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
    [%expr [%e expr_id ~loc (schema_id ~loc (Lident name))]]
  (* Unsupported *)
  | core_type ->
    Ppxlib.Location.raise_errorf ~loc "schematic-ppx: non supported type %a"
      Pprintast.core_type core_type

and tuple_schema ~loc types =
  let args = List.map ~f:(encode_type ~loc) types in
  let init = [%expr Null] in
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
    name:string option ->
    type_declaration ->
    (core_type * variance) list ->
    expanded ->
    closed

  val combine : loc:location -> closed list -> structure_item list

  val expand_abstract_type :
    loc:location -> names:string list -> core_type -> expanded

  val expand_record :
    loc:location ->
    names:string list ->
    name:string ->
    label_declaration list ->
    expanded

  val expand_variant :
    loc:location ->
    names:string list ->
    constructor_declaration list ->
    expanded
end

module Schema : Expansion = struct
  type closed = value_binding * bool

  type expanded = expression * bool

  let expand_record ~loc ~names:_ ~name:_ fields =
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
            ( loc,
              requirement,
              expr_string ~loc (rename txt),
              schema,
              inline,
              specs )
        in
        List.map ~f:expand_field fields
      in
      let init = [%expr Null] in
      let f (loc, requirement, field, schema, inline, specs) rest =
        let schema =
          if inline then
            [%expr Inline [%e schema]]
          else
            [%expr Outline [%e schema]]
        in
        [%expr
          Object
            {
              description = [%e expr_option ~loc expr_string specs.description];
              examples = [%e expr_list ~loc specs.examples];
              field = [%e field];
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

  let expand_variant ~loc ~names:_ options =
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

  let expand_abstract_type ~loc ~names:_ t =
    ([%expr [%e encode_type ~loc t].descriptor], false)

  let close ~loc ~name t params (body, recursive) =
    let init =
      [%expr
        {
          id =
            Some
              [%e
                expr_string ~loc
                  (Option.value ~default:(rename t.ptype_name.txt) name)];
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

  let combine ~loc expanded =
    [
      {
        pstr_desc =
          Pstr_value
            ( ( if List.exists ~f:snd expanded then
                Recursive
              else
                Nonrecursive ),
              List.map ~f:fst expanded );
        pstr_loc = loc;
      };
    ]
end

let monadize ~loc monad idt =
  match idt.txt with
  | Lident n ->
    Ldot (Lapply (Lident (String.capitalize @@ name "monadic" n), monad), "t")
  | Ldot (left, n) ->
    Ldot
      ( Lapply (Ldot (left, String.capitalize @@ name "monadic" n), Lident "M"),
        "t" )
  | _ -> Ppxlib.Location.raise_errorf ~loc "Functor application found as a type"

module Monadic : Expansion = struct
  type closed = type_declaration

  type expanded = type_kind * core_type option

  let rec expand_type ~names t =
    let ptyp_desc =
      match t.ptyp_desc with
      | Ptyp_constr (idt, params) -> (
        match idt.txt with
        | Lident name when List.mem ~equal:String.equal names name ->
          Ptyp_constr ({ txt = Ldot (Lident "M", "t"); loc = t.ptyp_loc }, [ t ])
        | _ ->
          Ptyp_constr
            ( { idt with txt = monadize ~loc:t.ptyp_loc (Lident "M") idt },
              params ) )
      | Ptyp_tuple types -> Ptyp_tuple (List.map ~f:(expand_type ~names) types)
      | _ ->
        Ppxlib.Location.raise_errorf ~loc:t.ptyp_loc
          "schematic-ppx: unsupported type"
    in
    { t with ptyp_desc }

  let expand_record ~loc:_ ~names ~name:_ fields =
    let f ({ pld_type; _ } as field) =
      { field with pld_type = expand_type ~names pld_type }
    in
    let kind = Ptype_record (List.map ~f fields) in
    (kind, None)

  let expand_variant ~loc:_ ~names (options : constructor_declaration list) =
    let f option =
      let pcd_args =
        match option.pcd_args with
        | Pcstr_tuple types ->
          Pcstr_tuple (List.map ~f:(expand_type ~names) types)
        | Pcstr_record labels ->
          let f ({ pld_type; _ } as l) =
            { l with pld_type = expand_type ~names pld_type }
          in
          Pcstr_record (List.map ~f labels)
      in
      { option with pcd_args }
    in
    (Ptype_variant (List.map ~f options), None)

  let expand_abstract_type ~loc:_ ~names t =
    (Ptype_abstract, Some (expand_type ~names t))

  let close ~loc ~name:_ { ptype_name; _ } params (kind, manifest) =
    {
      ptype_name;
      ptype_params = params;
      ptype_cstrs = [];
      ptype_kind = kind;
      ptype_private = Public;
      ptype_manifest = manifest;
      ptype_loc = loc;
      ptype_attributes = [];
    }

  let combine ~loc types =
    let local_name = "Monadic_" ^ Int.to_string loc.loc_start.pos_lnum in
    let def =
      let funct =
        [%str
          module Monadics (M : Base.Monad.S) = struct
            [%%i { pstr_loc = loc; pstr_desc = Pstr_type (Recursive, types) }]
          end]
      in
      {
        pstr_desc =
          Pstr_module
            {
              pmb_name = { txt = local_name; loc };
              pmb_expr =
                {
                  pmod_desc = Pmod_structure funct;
                  pmod_loc = loc;
                  pmod_attributes = [];
                };
              pmb_attributes = [];
              pmb_loc = loc;
            };
        pstr_loc = loc;
      }
    and open_def =
      let m =
        {
          pmod_desc = Pmod_ident { txt = Lident local_name; loc };
          pmod_loc = loc;
          pmod_attributes = [];
        }
      in
      {
        pstr_desc =
          Pstr_open
            {
              popen_expr = m;
              popen_override = Fresh;
              popen_loc = loc;
              popen_attributes = [];
            };
        pstr_loc = loc;
      }
    and f { ptype_name; ptype_loc = loc; _ } =
      {
        pstr_desc =
          Pstr_module
            {
              pmb_name =
                {
                  ptype_name with
                  txt = String.capitalize @@ name "monadic" ptype_name.txt;
                };
              pmb_expr =
                {
                  pmod_desc =
                    Pmod_functor
                      ( { txt = "M"; loc },
                        Some
                          {
                            pmty_desc =
                              Pmty_ident
                                {
                                  txt = Ldot (Ldot (Lident "Base", "Monad"), "S");
                                  loc;
                                };
                            pmty_loc = loc;
                            pmty_attributes = [];
                          },
                        {
                          pmod_desc =
                            Pmod_structure
                              [%str
                                module Monadic = Monadics (M)

                                type t =
                                  [%t
                                    {
                                      ptyp_desc =
                                        Ptyp_constr
                                          ( {
                                              txt =
                                                Ldot
                                                  ( Lident "Monadic",
                                                    ptype_name.txt );
                                              loc;
                                            },
                                            [] );
                                      ptyp_loc = loc;
                                      ptyp_loc_stack = [];
                                      ptyp_attributes = [];
                                    }]];
                          pmod_loc = loc;
                          pmod_attributes = [];
                        } );
                  pmod_loc = loc;
                  pmod_attributes = [];
                };
              pmb_loc = loc;
              pmb_attributes = [];
            };
        pstr_loc = loc;
      }
    in
    def :: open_def :: List.map ~f types
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

  let expand_record ~loc ~names:_ ~name:_ fields =
    let f { pld_name; pld_type; pld_loc; _ } : object_field =
      let t = expand_type pld_type in
      let t = [%type: [%t t] Js_of_ocaml.Js.readonly_prop] in
      { pof_desc = Otag (pld_name, t); pof_loc = pld_loc; pof_attributes = [] }
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

  let expand_variant ~loc ~names:_ (options : constructor_declaration list) =
    if
      Option.is_some
      @@ List.find
           ~f:(function
             | { pcd_args = Pcstr_tuple []; _ } -> false
             | _ -> true)
           options
    then
      Ppxlib.Location.raise_errorf ~loc
        "schematic-ppx: variants with arguments are unsupported"
    else
      {
        ptyp_desc = Ptyp_constr ({ txt = Lident "string_js"; loc }, []);
        ptyp_loc = loc;
        ptyp_loc_stack = [];
        ptyp_attributes = [];
      }

  let expand_abstract_type ~loc:_ ~names:_ t = expand_type t

  let close ~loc ~name:_ { ptype_name = { loc = name_loc; txt }; _ } params js =
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

  let combine ~loc items =
    let converters { ptype_name = { txt; _ }; _ } =
      let name s =
        let prefix = String.sub ~pos:0 ~len:(String.length txt - 2) txt in
        prefix ^ s
      in
      let fname n =
        {
          ppat_attributes = [];
          ppat_desc = Ppat_var { txt = name n; loc };
          ppat_loc = loc;
          ppat_loc_stack = [];
        }
      and schema =
        {
          pexp_attributes = [];
          pexp_desc = Pexp_ident { txt = Lident (name "schema"); loc };
          pexp_loc = loc;
          pexp_loc_stack = [];
        }
      and js =
        {
          ptyp_attributes = [];
          ptyp_desc = Ptyp_constr ({ txt = Lident (name "js"); loc }, []);
          ptyp_loc = loc;
          ptyp_loc_stack = [];
        }
      in
      [%stri
        let [%p fname "to_js"], [%p fname "of_js"], [%p fname "of_js_exn"] =
          let to_js v =
            ( Js_of_ocaml.Js.Unsafe.coerce
              @@ Schematic_jsoo.Jsoo.encode [%e schema] v
              : [%t js] )
          and of_js (js : [%t js]) =
            Schematic_jsoo.Jsoo.decode [%e schema]
              (Js_of_ocaml.Js.Unsafe.coerce js)
          in
          let of_js_exn (js : [%t js]) =
            match of_js js with
            | Result.Ok v -> v
            | Result.Error (`Js_decode m) ->
              Js_of_ocaml.Js.raise_js_error
                ((fun (type res t0)
                      (t1 : (t0 -> res Js_of_ocaml.Js.t) Js_of_ocaml.Js.constr)
                      (t0 : t0) (_ : unit) ->
                   ( Js_of_ocaml.Js.Unsafe.new_obj t1
                       [| Js_of_ocaml.Js.Unsafe.inject t0 |]
                     : res Js_of_ocaml.Js.t ))
                   Js_of_ocaml.Js.error_constr (Js_of_ocaml.Js.string m) ())
          in
          (to_js, of_js, of_js_exn)]
    in
    { pstr_desc = Pstr_type (Recursive, items); pstr_loc = loc }
    :: List.map ~f:converters items
end

module Expander (Expansion : Expansion) = struct
  let expand ~loc recursive t =
    let names =
      let f { ptype_name; _ } = ptype_name.txt in
      if recursive then
        List.map ~f t
      else
        []
    in
    let f t =
      let name =
        let rec extract_attributes = function
          | { attr_name = { txt = name; _ }; attr_loc = loc; attr_payload }
            :: attrs -> (
            match name with
            | "schematic.name" -> (
              match extract_attributes attrs with
              | None -> Some (expect_string_constant ~loc attr_payload)
              | Some _ ->
                Ppxlib.Location.raise_errorf ~loc
                  "schematic-ppx: duplicate schematic.name annotation on schema"
              )
            | _ ->
              let () = check_unknown_attribute ~loc name in
              extract_attributes attrs )
          | [] -> None
        in
        extract_attributes t.ptype_attributes
      in
      let body =
        let loc = t.ptype_loc in
        match t.ptype_kind with
        | Ptype_record fields ->
          Expansion.expand_record ~loc ~names ~name:t.ptype_name.txt fields
        | Ptype_abstract -> (
          match t.ptype_manifest with
          | Some t -> Expansion.expand_abstract_type ~loc ~names t
          | None -> failwith "no manifest on abstract type" )
        | Ptype_open -> failwith "unsupported Ptype_open"
        | Ptype_variant options -> Expansion.expand_variant ~loc ~names options
      in
      Expansion.close ~loc ~name t t.ptype_params body
    in
    let expanded = List.map ~f t in
    Expansion.combine ~loc expanded
end

module Jsoo_type_expander = Expander (Jsoo_type)
module Monadic_expander = Expander (Monadic)
module Schema_expander = Expander (Schema)

let expand ~loc ~path:_ ~inside (r, types) jsoo monadic =
  let r = r = Recursive in
  let contents =
    let contents = [] in
    let contents =
      if monadic then
        Monadic_expander.expand ~loc r types @ contents
      else
        contents
    in
    let contents =
      if jsoo then
        contents @ Jsoo_type_expander.expand ~loc r types
      else
        contents
    in
    Schema_expander.expand ~loc r types @ contents
  and namespace =
    if inside then
      [%str
        open Schema
        open Schemas

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
      let jsoo = flag "jsoo"
      and monadic = flag "monadic" in
      empty +> jsoo +> monadic
    in
    Deriving.Generator.make ~attributes:[] arg (expand ~inside)
  in
  Deriving.add ~str_type_decl "schema"

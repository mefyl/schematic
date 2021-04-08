open Base
open Poly
open Ppxlib

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

  let bool ~loc v =
    if v then
      [%expr true]
    else
      [%expr false]

  let string ~loc txt =
    {
      pexp_desc = Pexp_constant (Pconst_string (txt, loc, None));
      pexp_loc = loc;
      pexp_loc_stack = [];
      pexp_attributes = [];
    }

  let tuple ~loc = function
    | [] -> [%expr ()]
    | [ v ] -> v
    | exps ->
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

  let tuple ~loc = function
    | [] -> [%pat? ()]
    | [ v ] -> v
    | patterns ->
      {
        ppat_desc = Ppat_tuple patterns;
        ppat_loc = loc;
        ppat_loc_stack = [];
        ppat_attributes = [];
      }
end

module Warning = struct
  let make ~loc n =
    {
      attr_name = { txt = "warning"; loc };
      attr_payload =
        PStr
          [
            {
              pstr_desc =
                Pstr_eval
                  ( {
                      pexp_desc = Pexp_constant (Pconst_string (n, loc, None));
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

  let unused_rec ~loc = make ~loc "-39"
end

(* Caml variant constructor of the form [Foo of int * int] surprisingly do not
   have one tuple argument, but two individual int arguments, while [Foo of (int
   * int)] has a single tuple argument. Map the first case to the second and
   back.

   While we're at it, map constructor record arguments to tuple too, to
   streamline constructors arguments handling. *)

type normalized_ctor = {
  count : int;
  ctor_pattern : (int -> pattern) -> pattern;
  ctor_value : (int -> core_type -> expression) -> expression;
  pattern : (int -> pattern) -> pattern;
  value : (int -> core_type -> expression) -> expression;
  type_ : core_type;
}

let constructor_arguments_normalize ~loc = function
  | Pcstr_tuple []
  | Pcstr_record [] ->
    None
  | Pcstr_tuple [ type_ ] ->
    let pattern f = f 0
    and value f = f 0 type_ in
    Some
      {
        count = 1;
        ctor_pattern = pattern;
        ctor_value = value;
        pattern;
        type_;
        value;
      }
  | Pcstr_tuple types ->
    let type_ = ptyp ~loc ~desc:(Ptyp_tuple types)
    and pattern f =
      let f i _ = f i in
      Pattern.tuple ~loc (List.mapi ~f types)
    and value f = Expr.tuple ~loc (List.mapi ~f types) in
    Some
      {
        count = List.length types;
        ctor_pattern = pattern;
        ctor_value = value;
        pattern;
        type_;
        value;
      }
  | Pcstr_record fields ->
    let types =
      let f { pld_type; _ } = pld_type in
      List.map ~f fields
    and ctor_pattern f =
      ppat ~loc
        ~desc:
          (Ppat_record
             ( List.mapi
                 ~f:(fun i v -> ({ txt = Lident v.pld_name.txt; loc }, f i))
                 fields,
               Closed ))
    and ctor_value f =
      pexp ~loc
        ~desc:
          (Pexp_record
             ( List.mapi
                 ~f:(fun i { pld_name; pld_type; pld_loc = loc; _ } ->
                   ({ txt = Lident pld_name.txt; loc }, f i pld_type))
                 fields,
               None ))
    in
    let type_, pattern, value =
      match types with
      | [ type_ ] ->
        let pattern f = f 0
        and value f = f 0 type_ in
        (type_, pattern, value)
      | types ->
        let pattern f =
          let f i _ = f i in
          Pattern.tuple ~loc (List.mapi ~f types)
        and value f = Expr.tuple ~loc (List.mapi ~f types) in
        (ptyp ~loc ~desc:(Ptyp_tuple types), pattern, value)
    in
    Some
      {
        count = List.length fields;
        ctor_pattern;
        ctor_value;
        pattern;
        type_;
        value;
      }

let pattern_id ~loc txt =
  {
    ppat_desc = Ppat_var { txt; loc };
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

let expr_list ~loc list =
  let f m l = [%expr [%e m] :: [%e l]]
  and init = [%expr []] in
  List.fold_right ~f ~init list

let expr_int ~loc i =
  {
    pexp_desc = Pexp_constant (Pconst_integer (Int.to_string i, None));
    pexp_loc = loc;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

let expect_string ~loc = function
  | Pexp_constant (Pconst_string (value, _, _)) -> value
  | _ -> Ppxlib.Location.raise_errorf ~loc "schematic-ppx: string expected"

let expect_string_constant ~loc = function
  | PStr
      [%str [%e? { pexp_desc = Pexp_constant (Pconst_string (txt, _, _)); _ }]]
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

module Attributes = struct
  type t = {
    alias : string option;
    name : string option;
    union_key : string option;
  }

  let check_unknown_attribute ~loc name =
    if String.is_prefix ~prefix:"schematic." name then
      Ppxlib.Location.raise_errorf ~loc
        "schematic-ppx: unknown schematic annotation %s" name

  let rec extract ?(alias = false) ?(name = false) ?(union_key = false) =
    let merge name loc old v =
      match old with
      | None -> Some v
      | Some _ ->
        Ppxlib.Location.raise_errorf ~loc
          "schematic-ppx: duplicate schematic.%s annotation on schema" name
    in
    function
    | { attr_name = { txt; _ }; attr_loc = loc; attr_payload } :: attrs -> (
      let attrs = extract attrs in
      match txt with
      | "schematic.as" when alias -> (
        match attrs with
        | { alias = None; _ } ->
          { attrs with alias = Some (expect_string_constant ~loc attr_payload) }
        | _ ->
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: duplicate schematic.as annotation on schema")
      | "schematic.name" when name -> (
        match attrs with
        | { name = None; _ } ->
          { attrs with name = Some (expect_string_constant ~loc attr_payload) }
        | _ ->
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: duplicate schematic.name annotation on schema")
      | "schematic.union_key" when union_key ->
        {
          attrs with
          union_key =
            merge "union_key" loc attrs.union_key
            @@ expect_string_constant ~loc attr_payload;
        }
      | _ ->
        let () = check_unknown_attribute ~loc txt in
        attrs)
    | [] -> { alias = None; name = None; union_key = None }
end

type context = {
  attributes : Attributes.t;
  type_ : string;
  types : string list;
}

let suffix_id ~loc suffix = function
  | Lident n -> Lident (name suffix n)
  | Ldot (q, n) -> Ldot (q, name suffix n)
  | Lapply (_, _) ->
    Ppxlib.Location.raise_errorf ~loc "Functor application found as a type"

let schema_id ~loc = suffix_id ~loc "schema"

let expr_option ~loc expr = function
  | None -> [%expr None]
  | Some e -> [%expr Some [%e expr ~loc e]]

module Source = struct
  (** Where a configuration item came from: generic attributes versus schematic
      specific (eg. @default vs @schematic.default) *)
  type 'a t =
    | Generic of 'a
    | Schematic of 'a

  let value = function
    | Generic v
    | Schematic v ->
      v

  let generic v = Generic v

  let schematic v = Schematic v

  let merge ~loc name l r =
    match (l, r) with
    | None, v -> v
    | Some (Generic _), (Schematic _ as v) -> v
    | Some (Schematic _ as v), Generic _ -> v
    | _ ->
      Ppxlib.Location.raise_errorf ~loc
        "schematic-ppx: duplicate value for attribute %S" name
end

type specs = {
  default : expression Source.t option;
  description : string option;
  examples : expression list;
  maximum : int option;
  minimum : int option;
  omit : bool;
}

let specs_empty =
  {
    default = None;
    description = None;
    examples = [];
    maximum = None;
    minimum = None;
    omit = false;
  }

module type EXPANSION = sig
  type closed

  type expanded

  val close :
    loc:location ->
    context:context ->
    type_declaration ->
    (core_type * (variance * injectivity)) list ->
    expanded ->
    closed

  val combine : loc:location -> closed list -> structure_item list

  val expand_abstract_type :
    loc:location -> context:context -> core_type -> expanded

  val expand_record :
    loc:location -> context:context -> label_declaration list -> expanded

  val expand_variant :
    loc:location -> context:context -> constructor_declaration list -> expanded
end

module Schema : EXPANSION = struct
  type closed = value_binding

  type expanded = expression

  let rec encode_type ~loc ~context = function
    (* Mapping *)
    | [%type: (string * [%t? t]) list] ->
      [%expr
        {
          Schema.id = None;
          descriptor = Mapping [%e encode_type ~loc ~context t];
        }]
    (* Plain type *)
    | { ptyp_desc = Ptyp_constr ({ txt; loc }, []); _ } ->
      expr_id ~loc (schema_id ~loc txt)
    (* Parametric type *)
    | { ptyp_desc = Ptyp_constr ({ txt; loc }, args); _ } -> (
      match (txt, args) with
      | Lident "list", [ arg ] ->
        [%expr
          {
            Schema.id = None;
            descriptor = List [%e encode_type ~loc ~context arg];
          }]
      | _ ->
        let schema = expr_id ~loc (schema_id ~loc txt)
        and f t = (Nolabel, encode_type ~loc ~context t) in
        {
          pexp_desc = Pexp_apply (schema, List.map ~f args);
          pexp_loc = loc;
          pexp_loc_stack = [];
          pexp_attributes = [];
        })
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
          (tuple_schema ~loc ~context types)
      in
      [%expr { Schema.id = None; descriptor = [%e body] }]
    (* Type variable *)
    | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ } ->
      [%expr [%e expr_id ~loc (schema_id ~loc (Lident name))]]
    (* Polymorphic variant *)
    | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc = loc; _ } ->
      let desc = expand_polymorphic_variant ~loc ~context fields in
      [%expr { Schema.id = None; descriptor = [%e desc] }]
    (* Unsupported *)
    | core_type ->
      Ppxlib.Location.raise_errorf ~loc "schematic-ppx: unsupported type %a"
        Pprintast.core_type core_type

  and tuple_schema ~loc ~context types =
    let items =
      let args = List.map ~f:(encode_type ~loc ~context) types in
      let init = [%expr ItemEnd] in
      let f r l = [%expr Item { schema = [%e r]; rest = [%e l] }] in
      List.fold_right ~f ~init args
    in
    [%expr Tuple [%e items]]

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

  and expand_record ~loc ~context fields =
    _expand_record false ~loc ~context fields

  and _expand_record as_tuple ~loc ~context fields =
    let fields_schema =
      let expand_fields fields =
        let expand_field = function
          | {
              pld_attributes;
              pld_loc = loc;
              pld_name = { txt; _ };
              pld_type;
              _;
            } ->
            let schema, txt, inline, specs =
              let read_attribute
                  (schema, txt, inline, specs)
                  { attr_name; attr_loc = loc; attr_payload } =
                let default f req omit =
                  match attr_payload with
                  | PStr [%str [%e? exp]] ->
                    let default =
                      if req then
                        [%expr Some [%e exp]]
                      else
                        exp
                    in
                    ( schema,
                      txt,
                      inline,
                      {
                        specs with
                        default =
                          Some
                            (Source.merge ~loc "default" specs.default
                               (f default));
                        omit;
                      } )
                  | _ ->
                    Ppxlib.Location.raise_errorf ~loc
                      "schematic-ppx: expression payload expected"
                in
                match attr_name.txt with
                | "schematic.schema" -> (
                  match attr_payload with
                  | PStr [%str [%e? exp]] -> (Some exp, txt, inline, specs)
                  | _ ->
                    Ppxlib.Location.raise_errorf ~loc
                      "schematic-ppx: expression payload expected")
                | "default" -> default Source.generic true false
                | "schematic.default" -> default Source.schematic true false
                | "schematic.default_opt" ->
                  default Source.schematic false false
                | "schematic.omit" -> default Source.schematic true true
                | "schematic.omit_opt" -> default Source.schematic false true
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
                      | ( { txt = Lident name; loc = loc_name; _ },
                          ({ pexp_desc = value; pexp_loc = loc; _ } as desc) )
                        -> (
                        match name with
                        | "description" ->
                          {
                            specs with
                            description = Some (expect_string ~loc value);
                          }
                        | "default" ->
                          let () =
                            Fmt.epr
                              "%a Warning: deprecated, use @schematic.default\n"
                              Ppxlib.Location.print loc_name
                          in
                          {
                            specs with
                            default =
                              Some
                                (Source.merge ~loc:loc_name "default"
                                   specs.default
                                   (Source.schematic [%expr Some [%e desc]]));
                          }
                        | "example" -> { specs with examples = [ desc ] }
                        | "minimum" ->
                          { specs with maximum = Some (expect_int ~loc value) }
                        | "maximum" ->
                          { specs with minimum = Some (expect_int ~loc value) }
                        | "unique" -> specs
                        | _ ->
                          Ppxlib.Location.raise_errorf ~loc:loc_name
                            "schematic-ppx: unrecognized property %S" name)
                      | _ ->
                        Ppxlib.Location.raise_errorf ~loc
                          "schematic-ppx: unrecognized property"
                    in
                    ( schema,
                      txt,
                      inline,
                      List.fold ~init:specs ~f:prop properties )
                  | _ ->
                    Ppxlib.Location.raise_errorf ~loc
                      "schematic-ppx: record payload expected")
                | "schematic.as" ->
                  ( schema,
                    expect_string_constant ~loc attr_payload,
                    inline,
                    specs )
                | "schematic.inline" -> (
                  match attr_payload with
                  | PStr [] ->
                    if not inline then
                      (schema, txt, true, specs)
                    else
                      Ppxlib.Location.raise_errorf ~loc
                        "schematic-ppx: inline specified multiple times"
                  | _ ->
                    Ppxlib.Location.raise_errorf ~loc
                      "schematic-ppx: no payload expected")
                | name ->
                  let () = Attributes.check_unknown_attribute ~loc name in
                  (schema, txt, inline, specs)
              in
              List.fold_left ~f:read_attribute
                ~init:(None, txt, false, specs_empty)
                pld_attributes
            in
            let requirement, t =
              match pld_type with
              | {
               ptyp_desc = Ptyp_constr ({ txt = Lident "option"; _ }, [ t ]);
               _;
              } ->
                ([%expr Optional], t)
              | t -> (
                match specs.default with
                | Some default ->
                  ( [%expr
                      match [%e Source.value default] with
                      | Some default -> Default default
                      | None -> Required],
                    t )
                | None -> ([%expr Required], t))
            in
            let schema =
              Option.value ~default:(encode_type ~loc ~context t) schema
            in
            ( loc,
              requirement,
              Expr.string ~loc (rename txt),
              schema,
              inline,
              specs )
        in
        List.map ~f:expand_field fields
      in
      let init = [%expr FieldEnd] in
      let f (loc, requirement, field, schema, inline, specs) rest =
        let schema =
          if inline then
            [%expr Inline [%e schema]]
          else
            [%expr Outline [%e schema]]
        in
        [%expr
          Field
            {
              description = [%e expr_option ~loc Expr.string specs.description];
              examples = [%e expr_list ~loc specs.examples];
              field = [%e field];
              maximum = [%e expr_option ~loc expr_int specs.maximum];
              minimum = [%e expr_option ~loc expr_int specs.minimum];
              omit = [%e Expr.bool ~loc specs.omit];
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
      if as_tuple then
        match fields with
        | [ field ] -> f field |> snd
        | _ ->
          {
            pexp_desc = Pexp_tuple (List.map ~f:(fun x -> f x |> snd) fields);
            pexp_loc = loc;
            pexp_loc_stack = [];
            pexp_attributes = [];
          }
      else
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
      if as_tuple then
        match fields with
        | [ field ] -> f field |> snd
        | _ ->
          {
            ppat_desc = Ppat_tuple (List.map ~f:(fun x -> f x |> snd) fields);
            ppat_loc = loc;
            ppat_loc_stack = [];
            ppat_attributes = [];
          }
      else
        {
          ppat_desc = Ppat_record (List.map ~f fields, Closed);
          ppat_loc = loc;
          ppat_loc_stack = [];
          ppat_attributes = [];
        }
    in
    combinator ~loc fields field_name constructed deconstructed
      [%expr Object [%e fields_schema]]

  (*here*)
  and expand_variant ~loc ~context options =
    let f
        {
          pcd_name = { txt; loc = _ };
          pcd_args;
          pcd_loc = loc;
          pcd_attributes = attrs;
          _;
        } =
      (txt, pcd_args, loc, attrs)
    in
    expand_any_variant ~loc ~context false @@ List.map ~f options

  and expand_polymorphic_variant ~loc ~context options =
    let f { prf_desc; prf_loc; prf_attributes } =
      match prf_desc with
      | Rtag ({ txt; _ }, _, types) ->
        (txt, Pcstr_tuple types, prf_loc, prf_attributes)
      | Rinherit { ptyp_loc = loc; _ } ->
        Ppxlib.Location.raise_errorf ~loc
          "schematic-ppx: inheriting polymorphic variants not supported"
    in
    expand_any_variant ~loc ~context true @@ List.map ~f options

  and expand_any_variant ~loc ~context poly options =
    let f (txt, pcd_args, loc, attrs) =
      let name = String.lowercase txt in
      let schema, ctor_pattern, ctor_value, pattern, value =
        match constructor_arguments_normalize ~loc pcd_args with
        | None ->
          let schema =
            let Attributes.{ alias; _ } =
              Attributes.extract ~alias:true attrs
            in
            [%expr
              {
                Schema.id = None;
                descriptor =
                  String_const
                    [%e Expr.string ~loc (Option.value ~default:name alias)];
              }]
          in
          (schema, None, None, None, None)
        | Some { ctor_pattern; ctor_value; pattern; type_; value; _ } ->
          let p i = Pattern.id ~loc (Fmt.str "f%i" i)
          and v i _ = Expr.id ~loc (Fmt.str "f%i" i) in
          let schema =
            match pcd_args with
            | Pcstr_record fields ->
              let descriptor = _expand_record true ~loc ~context fields in
              [%expr { Schema.id = None; descriptor = [%e descriptor] }]
            | _ -> encode_type ~loc ~context type_
          in
          ( schema,
            Some (ctor_pattern p),
            Some (ctor_value v),
            Some (pattern p),
            Some (value v) )
      in
      let construct =
        let pattern =
          {
            ppat_desc =
              (if poly then
                Ppat_variant (txt, pattern)
              else
                Ppat_construct ({ txt = Lident txt; loc }, ctor_pattern));
            ppat_loc = loc;
            ppat_loc_stack = [];
            ppat_attributes = [];
          }
        and value = Option.value ~default:[%expr ()] value in
        [%expr
          function
          | [%p pattern] -> Some [%e value]
          | _ -> None]
      and destruct =
        let value =
          {
            pexp_desc =
              (if poly then
                Pexp_variant (txt, value)
              else
                Pexp_construct ({ txt = Lident txt; loc }, ctor_value));
            pexp_loc = loc;
            pexp_loc_stack = [];
            pexp_attributes = [];
          }
        and pattern = Option.value ~default:[%pat? ()] pattern in
        [%expr
          function
          | [%p pattern] -> [%e value]]
      in
      [%expr
        Schema.Case
          {
            name = [%e Expr.string ~loc name];
            schema = [%e schema];
            encoder = [%e construct];
            decoder = [%e destruct];
          }]
    in
    let key =
      match context.attributes.union_key with
      | None -> [%expr None]
      | Some name -> [%expr Some [%e Expr.string ~loc name]]
    in
    [%expr
      Schema.Union
        { cases = [%e expr_list ~loc (List.map ~f options)]; key = [%e key] }]

  let expand_abstract_type ~loc ~context t =
    [%expr [%e encode_type ~loc ~context t].descriptor]

  let close ~loc ~context t params body =
    let init =
      [%expr
        {
          Schema.id =
            Some
              [%e
                Expr.string ~loc
                  (Option.value ~default:(rename t.ptype_name.txt)
                     context.attributes.name)];
          descriptor = [%e body];
        }]
    and wrap arg exp =
      match arg with
      | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ }, _ ->
        [%expr fun [%p pattern_id ~loc (schema_name name)] -> [%e exp]]
      | { ptyp_desc = Ptyp_any; _ }, _ -> [%expr fun _ -> [%e exp]]
      | t, _ ->
        Ppxlib.Location.raise_errorf ~loc
          "schematic-ppx: unsupported type parameter %a" Pprintast.core_type t
    in
    let body = List.fold_right ~f:wrap ~init params in
    let name = schema_name t.ptype_name.txt in
    let pattern = pattern_id ~loc:t.ptype_name.loc name in
    let body =
      {
        pvb_pat = pattern;
        pvb_expr = body;
        pvb_loc = loc;
        pvb_attributes = [ Warning.unused_rec ~loc ];
      }
    in
    body

  let combine ~loc expanded =
    [ { pstr_desc = Pstr_value (Recursive, expanded); pstr_loc = loc } ]
end

module Jsoo_type : EXPANSION = struct
  type closed = type_declaration

  type expanded = core_type

  let expand_ident = function
    | Lident n -> Lident (name "js" n)
    | Ldot (ident, m) -> Ldot (ident, name "js" m)
    | Lapply _ -> failwith "Lapply"

  let rec expand_type ~loc ~context t =
    match t.ptyp_desc with
    | Ptyp_constr (name, args) ->
      {
        t with
        ptyp_desc =
          Ptyp_constr
            ( { name with txt = expand_ident name.txt },
              List.map ~f:(expand_type ~loc ~context) args );
      }
    (* Polymorphic variant *)
    | Ptyp_variant (fields, _, _) ->
      expand_polymorphic_variant ~loc ~context fields
    (* I don't know of any way to properly type this, so any it is *)
    | Ptyp_tuple _ -> [%type: Js_of_ocaml.Js.Unsafe.any]
    (* Type variable *)
    | Ptyp_var _ -> t
    | _ ->
      Ppxlib.Location.raise_errorf ~loc:t.ptyp_loc
        "schematic-ppx: unsupported type %a" Pprintast.core_type t

  and expand_record ~loc ~context fields =
    let f { pld_name; pld_type; pld_loc; _ } : object_field =
      let t = expand_type ~loc ~context pld_type in
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

  and expand_variant ~loc ~context options =
    let f
        {
          pcd_name = { txt; loc = _ };
          pcd_args;
          pcd_loc = loc;
          pcd_attributes = attrs;
          _;
        } =
      (txt, pcd_args, loc, attrs)
    in
    expand_any_variant ~loc ~context false @@ List.map ~f options

  and expand_polymorphic_variant ~loc ~context options =
    let f { prf_desc; prf_loc; prf_attributes } =
      match prf_desc with
      | Rtag ({ txt; _ }, _, types) ->
        (txt, Pcstr_tuple types, prf_loc, prf_attributes)
      | Rinherit { ptyp_loc = loc; _ } ->
        Ppxlib.Location.raise_errorf ~loc
          "schematic-ppx: inheriting polymorphic variants not supported"
    in
    expand_any_variant ~loc ~context true @@ List.map ~f options

  and expand_any_variant ~loc ~context:_ _ options =
    if
      Option.is_some
      @@ List.find
           ~f:(function
             | _, Pcstr_tuple [], _, _ -> false
             | _ -> true)
           options
    then
      [%type: Js_of_ocaml.Js.Unsafe.any]
    else
      {
        ptyp_desc = Ptyp_constr ({ txt = Lident "string_js"; loc }, []);
        ptyp_loc = loc;
        ptyp_loc_stack = [];
        ptyp_attributes = [];
      }

  let expand_abstract_type ~loc ~context t = expand_type ~loc ~context t

  let close
      ~loc ~context:_ { ptype_name = { loc = name_loc; txt }; _ } params js =
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
    let converters { ptype_name = { txt; _ }; ptype_params; _ } =
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
          ptyp_desc =
            Ptyp_constr
              ( { txt = Lident (name "js"); loc },
                List.map
                  ~f:(fun _ ->
                    {
                      ptyp_desc = Ptyp_any;
                      ptyp_loc = loc;
                      ptyp_loc_stack = [];
                      ptyp_attributes = [];
                    })
                  ptype_params );
          ptyp_loc = loc;
          ptyp_loc_stack = [];
        }
      in
      let apply, parametrize =
        let params =
          let f ({ ptyp_desc; ptyp_loc = loc; _ }, _) =
            match ptyp_desc with
            | Ptyp_any -> None
            | Ptyp_var n -> Some (loc, Fmt.str "%s_schema" n)
            | _ ->
              Ppxlib.Location.raise_errorf ~loc
                "schematic-ppx: unsupported type parameter"
          in
          List.filter_map ~f ptype_params
        in
        let apply e =
          let args =
            let f (loc, name) = Expr.id ~loc name in
            List.map ~f params
          in
          Expr.apply ~loc e args
        and parametrize e =
          let f (loc, name) f =
            let pattern = Pattern.id ~loc name in
            [%expr fun [%p pattern] -> [%e f]]
          in
          List.fold_right ~init:e ~f params
        in
        (apply, parametrize)
      in
      let to_js =
        let body =
          [%expr
            fun v : [%t js] ->
              Js_of_ocaml.Js.Unsafe.coerce
              @@ Schematic_jsoo.Jsoo.encode [%e apply schema] v]
        in
        parametrize body
      and of_js =
        let body =
          [%expr
            fun (js : [%t js]) ->
              Schematic_jsoo.Jsoo.decode [%e apply schema]
                (Js_of_ocaml.Js.Unsafe.coerce js)]
        in
        parametrize body
      and of_js_exn =
        let body =
          [%expr
            fun (js : [%t js]) ->
              match [%e apply [%expr of_js]] js with
              | Result.Ok v -> v
              | Result.Error e ->
                Js_of_ocaml.Js.raise_js_error
                  ((fun (type res t0)
                        (t1 :
                          (t0 -> res Js_of_ocaml.Js.t) Js_of_ocaml.Js.constr)
                        (t0 : t0) (_ : unit) : res Js_of_ocaml.Js.t ->
                     Js_of_ocaml.Js.Unsafe.new_obj t1
                       [| Js_of_ocaml.Js.Unsafe.inject t0 |])
                     Js_of_ocaml.Js.error_constr
                     (Js_of_ocaml.Js.string
                     @@ Schematic.Coding.error_to_string e)
                     ())]
        in
        parametrize body
      in
      [%stri
        let [%p fname "to_js"], [%p fname "of_js"], [%p fname "of_js_exn"] =
          let to_js = [%e to_js]
          and of_js = [%e of_js] in
          let of_js_exn = [%e of_js_exn] in
          (to_js, of_js, of_js_exn)]
    in
    { pstr_desc = Pstr_type (Recursive, items); pstr_loc = loc }
    :: List.map ~f:converters items
end

module Expander (Expansion : EXPANSION) = struct
  let expand ~loc recursive t =
    let names =
      let f { ptype_name; _ } = ptype_name.txt in
      if recursive then
        List.map ~f t
      else
        []
    in
    let f t =
      let context =
        {
          attributes =
            Attributes.extract ~name:true ~union_key:true t.ptype_attributes;
          type_ = t.ptype_name.txt;
          types = names;
        }
      in
      let () =
        match (context.attributes.union_key, t.ptype_kind) with
        | None, _
        | Some _, Ptype_variant _ ->
          ()
        | _ ->
          Ppxlib.Location.raise_errorf ~loc:t.ptype_loc
            "schematic-ppx: union_key is only valid on variants"
      in
      let body =
        let loc = t.ptype_loc in
        match t.ptype_kind with
        | Ptype_record fields -> Expansion.expand_record ~loc ~context fields
        | Ptype_abstract -> (
          match t.ptype_manifest with
          | Some core_type ->
            Expansion.expand_abstract_type ~loc ~context core_type
          | None -> failwith "no manifest on abstract type")
        | Ptype_open -> failwith "unsupported Ptype_open"
        | Ptype_variant options ->
          Expansion.expand_variant ~loc ~context options
      in
      Expansion.close ~loc ~context t t.ptype_params body
    in
    let expanded = List.map ~f t in
    Expansion.combine ~loc expanded
end

module Jsoo_type_expander = Expander (Jsoo_type)
module Schema_expander = Expander (Schema)

let expand ~loc ~path:_ ~inside (r, types) jsoo =
  let r = r = Recursive in
  let contents =
    let contents = [] in
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
        open Schemas

        let _ = unit_schema]
    else
      [%str
        module Schema = Schematic.Schema
        open Schematic.Schema.Schemas

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

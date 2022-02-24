open Base
open Poly
open Ppxlib

type inline =
  | Inline
  | Inline_try
  | Outline

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

module Ast_pattern = struct
  include Ast_pattern

  let ( let+ ) v f = map1 ~f v

  let drop =
    let f _ _ _ cont = cont in
    of_func f

  let econstraint expression type_ =
    let expression = to_func expression
    and type_ = to_func type_ in
    let f context loc pl cont =
      match pl with
      | { pexp_desc = Pexp_constraint (exp, t); _ } ->
        expression context exp.pexp_loc exp (fun exp ->
            type_ context t.ptyp_loc t (fun t -> cont exp t))
      | _ -> Location.raise_errorf ~loc "type constraint expected"
    in
    of_func f
end

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
  let make ~loc pexp_desc =
    { pexp_desc; pexp_loc = loc; pexp_loc_stack = []; pexp_attributes = [] }

  let id ~loc name = make ~loc (Pexp_ident { txt = Lident name; loc })
  let bool ~loc v = if v then [%expr true] else [%expr false]

  let list ~loc list =
    let f m l = [%expr [%e m] :: [%e l]]
    and init = [%expr []] in
    List.fold_right ~f ~init list

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

  let some ~loc e = [%expr Some [%e e]]
  let none ~loc = [%expr None]
end

module Pattern = struct
  let make ~loc ppat_desc =
    { ppat_desc; ppat_loc = loc; ppat_loc_stack = []; ppat_attributes = [] }

  let id ~loc name = make ~loc (Ppat_var { txt = name; loc })

  let list ~loc list =
    let f m l = [%pat? [%p m] :: [%p l]]
    and init = [%pat? []] in
    List.fold_right ~f ~init list

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

module Type = struct
  let make ~loc ptyp_desc =
    { ptyp_desc; ptyp_loc_stack = []; ptyp_loc = loc; ptyp_attributes = [] }

  let id ~loc ?(parameters = []) name =
    make ~loc @@ Ptyp_constr ({ loc; txt = Lident name }, parameters)
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

  let unused_var_strict ~loc = make ~loc "-unused-var-strict"
  let unused_rec ~loc = make ~loc "-unused-rec-flag"
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
  | Pcstr_tuple [] | Pcstr_record [] -> None
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
      Expr.make ~loc
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

let expr_int ~loc i =
  {
    pexp_desc = Pexp_constant (Pconst_integer (Int.to_string i, None));
    pexp_loc = loc;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

let expect ~loc pattern ast = Ast_pattern.parse pattern loc ast Fn.id

let rename name =
  let len = String.length name in
  if name.[len - 1] = '_' then String.sub name ~pos:0 ~len:(len - 1) else name

let name suffix s =
  let s = rename s in
  if s = "t" then suffix else s ^ "_" ^ suffix

let suffix_id ~loc suffix = function
  | Lident n -> Lident (name suffix n)
  | Ldot (q, n) -> Ldot (q, name suffix n)
  | Lapply (_, _) ->
    Ppxlib.Location.raise_errorf ~loc "Functor application found as a type"

let expr_option ~loc expr = function
  | None -> [%expr None]
  | Some e -> [%expr Some [%e expr ~loc e]]

module Source = struct
  (** Where a configuration item came from: generic attributes versus schematic
      specific (eg. \@default vs \@schematic.default) *)
  type 'a t =
    | Generic of 'a
    | Schematic of 'a

  let value = function Generic v | Schematic v -> v
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

module Attributes = struct
  type field = {
    alias : string option;
    computed : expression option;
    inline : inline option;
    loc : location;
    name : string; [@main]
    schema :
      (expression * (Semver.t * [ `Eq | `Gt | `Gte | `Lt | `Lte ]) option) list;
    specs : specs; [@default specs_empty]
    sqml_primary : bool option;
    sqml_type : expression option;
    type_ : core_type;
  }
  [@@deriving make]

  type t = {
    alias : string option;
    computed :
      (string, expression * core_type, String.comparator_witness) Map.t;
    name : expression option;
    union_key : string option;
  }

  let check_unknown_attribute ~loc name =
    match String.chop_prefix ~prefix:"schematic." name with
    | Some name ->
      Ppxlib.Location.raise_errorf ~loc
        "schematic-ppx: unknown schematic annotation %s" name
    | None -> ()

  let rec extract ?(alias = false) ?(computed = false) ?(name = false)
      ?(union_key = false) =
    let merge name loc old v =
      match old with
      | None -> Some v
      | Some _ ->
        Ppxlib.Location.raise_errorf ~loc
          "schematic-ppx: duplicate schematic.%s annotation on schema" name
    in
    function
    | { attr_name = { txt; _ }; attr_loc = loc; attr_payload } :: attrs -> (
      let attrs = extract ~alias ~computed ~name ~union_key attrs in
      match txt with
      | "schematic.as" when alias -> (
        match attrs with
        | { alias = None; _ } ->
          {
            attrs with
            alias =
              Some
                (expect
                   Ast_pattern.(single_expr_payload (estring __))
                   ~loc attr_payload);
          }
        | _ ->
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: duplicate schematic.as annotation on schema")
      | "schematic.computed" when computed ->
        let computed =
          let computed =
            expect ~loc
              Ast_pattern.(
                single_expr_payload
                  (pexp_record
                     (many
                        (pair (loc (lident __)) (econstraint __ __ |> pack2)
                        |> pack2))
                     drop))
              attr_payload
          in
          match Map.of_alist (module String) computed with
          | `Ok computed -> computed
          | `Duplicate_key key ->
            Ppxlib.Location.raise_errorf ~loc
              "schematic-ppx: duplicate schematic.computed key: %s" key
        and combine ~key _ _ =
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: duplicate schematic.computed key: %s" key
        in
        {
          attrs with
          computed = Map.merge_skewed ~combine attrs.computed computed;
        }
      | "schematic.name" when name -> (
        match attrs with
        | { name = None; _ } ->
          {
            attrs with
            name =
              Some
                (expect ~loc Ast_pattern.(single_expr_payload __) attr_payload);
          }
        | _ ->
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: duplicate schematic.name annotation on schema")
      | "schematic.union_key" when union_key ->
        {
          attrs with
          union_key =
            merge "union_key" loc attrs.union_key
            @@ expect ~loc
                 Ast_pattern.(single_expr_payload (estring __))
                 attr_payload;
        }
      | _ ->
        let () = check_unknown_attribute ~loc txt in
        attrs)
    | [] ->
      {
        alias = None;
        computed = Map.empty (module String);
        name = None;
        union_key = None;
      }
end

type context = {
  code_path : Ppxlib.Code_path.t;
  inside : bool;
  loc : location;
  recursive : bool;
  types : string list;
}

type type_context = {
  attributes : Attributes.t;
  loc : location;
  name : string;
  params : (core_type * (variance * injectivity)) list; (* type_ : string; *)
}

and type_ =
  | Abstract
  | Core of core_type
  | Record of Attributes.field list
  | Variant of constructor_declaration list

module type EXPANSION = sig
  type closed
  type configuration
  type expanded

  val close :
    configuration:configuration ->
    context:context ->
    type_context:type_context ->
    expanded ->
    closed

  val combine :
    loc:location -> configuration -> closed list -> structure_item list

  val expand_abstract_type :
    context:context -> type_context:type_context -> core_type -> expanded

  val expand_record :
    context:context ->
    type_context:type_context ->
    Attributes.field list ->
    expanded

  val expand_variant :
    context:context ->
    type_context:type_context ->
    constructor_declaration list ->
    expanded
end

let expand_field
    { pld_attributes; pld_loc = loc; pld_name = { txt = name; _ }; pld_type; _ }
    =
  let read_attribute (attrs : Attributes.field)
      { attr_name; attr_loc = loc; attr_payload } =
    let default f req omit =
      match attr_payload with
      | PStr [%str [%e? exp]] ->
        let default = if req then [%expr Some [%e exp]] else exp in
        {
          attrs with
          specs =
            {
              attrs.specs with
              default =
                Some
                  (Source.merge ~loc "default" attrs.specs.default (f default));
              omit;
            };
        }
      | _ ->
        Ppxlib.Location.raise_errorf ~loc
          "schematic-ppx: expression payload expected"
    in
    match attr_name.txt with
    | "schematic.schema" ->
      let pattern =
        let open Ast_pattern in
        let versioned =
          let+ op, (schema, version) =
            let version v =
              let f context loc pl cont =
                parse (estring __) loc pl (fun s ->
                    match Semver.of_string s with
                    | None ->
                      Location.raise_errorf ~loc "invalid semantic version"
                    | Some version ->
                      (to_func v) context loc version (fun v -> cont v))
              in
              of_func f
            and op v =
              let f context loc pl cont =
                let v = to_func v in
                parse
                  (pexp_ident @@ lident __)
                  loc pl
                  (function
                    | "=" -> v context loc `Eq (fun v -> cont v)
                    | "<" -> v context loc `Lt (fun v -> cont v)
                    | "<=" -> v context loc `Lte (fun v -> cont v)
                    | ">" -> v context loc `Gt (fun v -> cont v)
                    | ">=" -> v context loc `Gte (fun v -> cont v)
                    | op ->
                      Location.raise_errorf ~loc "invalid version operator: %s"
                        op)
              in
              of_func f
            in
            pexp_apply (op __)
              (pair nolabel __ ^:: pair nolabel (version __) ^:: nil |> pack2)
            |> pack2
          in
          (schema, Some (version, op))
        and plain =
          let+ expr = __ in
          (expr, None)
        in
        single_expr_payload (versioned ||| plain)
      in
      { attrs with schema = expect ~loc pattern attr_payload :: attrs.schema }
    | "default" -> default Source.generic true false
    | "schematic.default" -> default Source.schematic true false
    | "schematic.default_opt" -> default Source.schematic false false
    | "schematic.omit" -> default Source.schematic true true
    | "schematic.omit_opt" -> default Source.schematic false true
    | "schematic.properties" -> (
      match attr_payload with
      | PStr
          [%str
            [%e?
              { pexp_desc = Pexp_record (properties, None); pexp_loc = loc; _ }]]
        ->
        let prop specs = function
          | { txt = Lident name; loc = loc_name; _ }, value -> (
            match name with
            | "description" ->
              {
                specs with
                description = Some (expect Ast_pattern.(estring __) ~loc value);
              }
            | "default" ->
              let () =
                Fmt.epr "%a Warning: deprecated, use @schematic.default\n"
                  Ppxlib.Location.print loc_name
              in
              {
                specs with
                default =
                  Some
                    (Source.merge ~loc:loc_name "default" specs.default
                       (Source.schematic [%expr Some [%e value]]));
              }
            | "example" -> { specs with examples = [ value ] }
            | "minimum" ->
              {
                specs with
                maximum = Some (expect ~loc Ast_pattern.(eint __) value);
              }
            | "maximum" ->
              {
                specs with
                minimum = Some (expect ~loc Ast_pattern.(eint __) value);
              }
            | "unique" -> specs
            | _ ->
              Ppxlib.Location.raise_errorf ~loc:loc_name
                "schematic-ppx: unrecognized property %S" name)
          | _ ->
            Ppxlib.Location.raise_errorf ~loc
              "schematic-ppx: unrecognized property"
        in
        { attrs with specs = List.fold ~init:attrs.specs ~f:prop properties }
      | _ ->
        Ppxlib.Location.raise_errorf ~loc
          "schematic-ppx: record payload expected")
    | "schematic.as" ->
      Ast_pattern.parse
        Ast_pattern.(single_expr_payload (estring __))
        loc attr_payload
        (fun alias -> { attrs with alias = Some alias })
    | ("schematic.inline" as attr) | ("schematic.inline_try" as attr) -> (
      match attr_payload with
      | PStr [] ->
        if Option.is_none attrs.inline then
          let v =
            if String.(attr = "schematic.inline") then Inline else Inline_try
          in
          { attrs with inline = Some v }
        else
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: inline specified multiple times"
      | _ ->
        Ppxlib.Location.raise_errorf ~loc "schematic-ppx: no payload expected")
    | "schematic.sqml.primary" ->
      if Option.is_some attrs.sqml_primary then
        Ppxlib.Location.raise_errorf ~loc
          "schematic-ppx: duplicate schematic.sqml annotation"
      else { attrs with sqml_primary = Some true }
    | "schematic.sqml.type" ->
      if Option.is_some attrs.sqml_type then
        Ppxlib.Location.raise_errorf ~loc
          "schematic-ppx: duplicate schematic.sqml annotation"
      else
        Ast_pattern.parse
          Ast_pattern.(single_expr_payload __)
          loc attr_payload
          (fun sqml_type -> { attrs with sqml_type = Some sqml_type })
    | name ->
      let () = Attributes.check_unknown_attribute ~loc name in
      attrs
  in
  List.fold_left ~f:read_attribute
    ~init:(Attributes.make_field ~loc ~type_:pld_type name)
    pld_attributes

module Schema = struct
  type closed = value_binding * value_binding
  type configuration = unit

  type expanded =
    | Components of {
        descriptor : expression;
        id : expression;
        parametric : expression;
      }
    | Expression of expression

  let schema_name s = name "schema" s
  let schema_name_versioned s = name "schema_versioned" s
  let schema_id_versioned ~loc = suffix_id ~loc "schema_versioned"

  let expanded_of_descriptor ~loc ?(id = [%expr None])
      ?(parametric = [%expr None]) descriptor =
    Components { descriptor; id; parametric }

  let schema_of_expanded ~loc = function
    | Components { descriptor; id; parametric } ->
      [%expr
        Schema.
          {
            descriptor = [%e descriptor];
            id = [%e id];
            parametric = [%e parametric];
          }]
    | Expression e -> e

  let rec encode_type ~context ~type_context =
    let loc = type_context.loc in
    function
    (* Mapping *)
    | [%type: (string * [%t? t]) list] ->
      expanded_of_descriptor ~loc
        [%expr
          Schema.Mapping
            [%e schema_of_expanded ~loc @@ encode_type ~context ~type_context t]]
    (* Plain type *)
    | { ptyp_desc = Ptyp_constr ({ txt; loc }, []); _ } ->
      Expression
        [%expr
          [%e expr_id ~loc @@ schema_id_versioned ~loc txt] __schematic_version]
    (* Lists *)
    | { ptyp_desc = Ptyp_constr ({ txt = Lident "list"; loc }, [ arg ]); _ } ->
      expanded_of_descriptor ~loc
        [%expr
          Schema.List
            [%e
              schema_of_expanded ~loc @@ encode_type ~context ~type_context arg]]
    (* Parametric type *)
    | { ptyp_desc = Ptyp_constr ({ txt; loc }, args); _ } ->
      let schema_f = expr_id ~loc (schema_id_versioned ~loc txt)
      and f t =
        ( Nolabel,
          schema_of_expanded ~loc @@ encode_type ~context ~type_context t )
      in
      Expression
        {
          pexp_desc =
            Pexp_apply
              ( schema_f,
                (Nolabel, [%expr __schematic_version]) :: List.map ~f args );
          pexp_loc = loc;
          pexp_loc_stack = [];
          pexp_attributes = [];
        }
    (* Tuple *)
    | { ptyp_desc = Ptyp_tuple types; _ } ->
      let field_name i { ptyp_loc = loc; _ } = (loc, "e" ^ Int.to_string i)
      and field_value i { ptyp_loc = loc; _ } =
        expr_id ~loc @@ Lident ("e" ^ Int.to_string i)
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
        combinator ~loc types field_name field_value constructed deconstructed
          (tuple_descriptor ~context ~type_context types)
      in
      expanded_of_descriptor ~loc body
    (* Type variable *)
    | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ } ->
      Expression (expr_id ~loc (schema_id_versioned ~loc (Lident name)))
    (* Polymorphic variant *)
    | { ptyp_desc = Ptyp_variant (fields, _, _); _ } ->
      expand_polymorphic_variant ~context ~type_context fields
    (* Unsupported *)
    | core_type ->
      Ppxlib.Location.raise_errorf ~loc "schematic-ppx: unsupported type %a"
        Pprintast.core_type core_type

  and tuple_descriptor ~context ~type_context types =
    let loc = type_context.loc in
    let items =
      let args =
        List.map
          ~f:(fun d ->
            schema_of_expanded ~loc @@ encode_type ~context ~type_context d)
          types
      in
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
      (int -> f -> expression) ->
      expression ->
      pattern ->
      expression ->
      expression =
   fun ~loc fields field_name field_value constructed deconstructed obj ->
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
        pexp_desc =
          Pexp_fun (Nolabel, None, pattern, [%expr Result.Ok [%e constructed]]);
        pexp_loc = loc;
        pexp_loc_stack = [];
        pexp_attributes = [];
      }
    and deconstruct =
      let value =
        let expr i f = field_value i f in
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
    [%expr Schema.Map ([%e construct], [%e deconstruct], [%e obj])]

  and expand_record ~context ~type_context fields =
    _expand_record false ~context ~type_context fields

  and _expand_record as_tuple ~context ~type_context
      (fields : Attributes.field list) =
    let loc = type_context.loc in
    let type_ =
      (* This type is used to explicitely disambiguate mapped values in case of
         record field name conflicts, otherwise a schema can end up manipulating
         a completely different type. *)
      ptyp ~loc
        ~desc:
          (Ptyp_constr
             ( { txt = Lident type_context.name; loc },
               List.map ~f:fst type_context.params ))
    in
    let fields_schema =
      let init = [%expr FieldEnd] in
      let f Attributes.{ alias; inline; loc; name; schema; specs; type_; _ }
          rest =
        let requirement, type_ =
          match type_ with
          | {
           ptyp_desc = Ptyp_constr ({ txt = Lident "option"; _ }, [ type_ ]);
           _;
          } ->
            ([%expr Optional], type_)
          | type_ -> (
            match specs.default with
            | Some default ->
              ( [%expr
                  match [%e Source.value default] with
                  | Some default -> Default default
                  | None -> Required],
                type_ )
            | None -> ([%expr Required], type_))
        in
        let schema =
          let f next = function
            | schema, None -> schema
            | schema, Some (Semver.{ major; minor; patch; _ }, op) ->
              let module Ast_builder = Ast_builder.Make (struct
                let loc = loc
              end) in
              let version =
                [%expr
                  Schematic.Version.make [%e Ast_builder.eint major]
                    [%e Ast_builder.eint minor] [%e Ast_builder.eint patch]]
              and predicate =
                match op with
                | `Eq -> [%expr Schematic.Version.equal]
                | `Gt -> [%expr Schematic.Version.greater_than]
                | `Gte ->
                  [%expr
                    fun l r ->
                      Schematic.Version.equal l r
                      || Schematic.Version.greater_than l r]
                | `Lt -> [%expr Schematic.Version.less_than]
                | `Lte ->
                  [%expr
                    fun l r ->
                      Schematic.Version.equal l r
                      || Schematic.Version.less_than l r]
              in
              [%expr
                match __schematic_version with
                | Some version when [%e predicate] version [%e version] ->
                  [%e schema]
                | _ -> [%e next]]
          in
          List.fold
            ~init:
              (schema_of_expanded ~loc
              @@ encode_type ~context ~type_context type_)
            ~f schema
        in
        let schema =
          match Option.value ~default:Outline inline with
          | Inline -> [%expr Inline [%e schema]]
          | Inline_try -> [%expr Inline_try [%e schema]]
          | Outline -> [%expr Outline [%e schema]]
        in
        [%expr
          Field
            {
              description = [%e expr_option ~loc Expr.string specs.description];
              examples = [%e Expr.list ~loc specs.examples];
              field =
                [%e
                  Expr.string ~loc (rename @@ Option.value ~default:name alias)];
              maximum = [%e expr_option ~loc expr_int specs.maximum];
              minimum = [%e expr_option ~loc expr_int specs.minimum];
              omit = [%e Expr.bool ~loc specs.omit];
              requirement = [%e requirement];
              rest = [%e rest];
              schema = [%e schema];
              title = None;
            }]
      in
      List.fold_right ~f ~init fields
    and field_name _ (field : Attributes.field) = (field.loc, field.name)
    and field_value _ (field : Attributes.field) =
      match field.computed with
      | None -> expr_id ~loc @@ Lident field.name
      | Some computed -> computed
    and constructed =
      let f (field : Attributes.field) =
        let value =
          {
            pexp_desc = Pexp_ident { txt = Lident field.name; loc };
            pexp_loc = field.loc;
            pexp_loc_stack = [];
            pexp_attributes = [];
          }
        in
        match field.computed with
        | None -> Some ({ txt = Lident field.name; loc }, value)
        | Some _ -> None
      in
      if as_tuple then
        match fields with
        | [ field ] -> (
          match f field with
          | Some (_, value) -> value
          | None ->
            Ppxlib.Location.raise_errorf ~loc
              "schematic-ppx: unexpected empty record")
        | _ ->
          {
            pexp_desc =
              Pexp_tuple
                (List.filter_map ~f:(fun x -> f x |> Option.map ~f:snd) fields);
            pexp_loc = loc;
            pexp_loc_stack = [];
            pexp_attributes = [];
          }
      else
        let constructed =
          {
            pexp_desc = Pexp_record (List.filter_map ~f fields, None);
            pexp_loc = loc;
            pexp_loc_stack = [];
            pexp_attributes = [];
          }
        in
        [%expr ([%e constructed] : [%t type_])]
    and deconstructed =
      let f (field : Attributes.field) =
        match field.computed with
        | Some _ -> None
        | None ->
          Some
            ( { txt = Lident field.name; loc },
              {
                ppat_desc = Ppat_var { txt = field.name; loc };
                ppat_loc = field.loc;
                ppat_loc_stack = [];
                ppat_attributes = [];
              } )
      in
      if as_tuple then
        match fields with
        | [ field ] -> (
          match f field with
          | Some (_, v) -> v
          | None ->
            Ppxlib.Location.raise_errorf ~loc
              "schematic-ppx: unexpected empty record")
        | _ ->
          {
            ppat_desc =
              Ppat_tuple
                (List.filter_map ~f:(fun x -> f x |> Option.map ~f:snd) fields);
            ppat_loc = loc;
            ppat_loc_stack = [];
            ppat_attributes = [];
          }
      else
        let deconstructed =
          {
            ppat_desc = Ppat_record (List.filter_map ~f fields, Closed);
            ppat_loc = loc;
            ppat_loc_stack = [];
            ppat_attributes = [];
          }
        in
        [%pat? ([%p deconstructed] : [%t type_])]
    in
    expanded_of_descriptor ~loc
    @@ combinator ~loc fields field_name field_value constructed deconstructed
         [%expr Object [%e fields_schema]]

  and expand_variant ~context ~type_context options =
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
    expand_any_variant ~context ~type_context false @@ List.map ~f options

  and expand_polymorphic_variant ~context ~type_context options =
    let f { prf_desc; prf_loc; prf_attributes } =
      match prf_desc with
      | Rtag ({ txt; _ }, _, types) ->
        (txt, Pcstr_tuple types, prf_loc, prf_attributes)
      | Rinherit { ptyp_loc = loc; _ } ->
        Ppxlib.Location.raise_errorf ~loc
          "schematic-ppx: inheriting polymorphic variants not supported"
    in
    expand_any_variant ~context ~type_context true @@ List.map ~f options

  and expand_any_variant ~context ~type_context poly options =
    let loc = type_context.loc in
    let f (txt, pcd_args, loc, attrs) =
      let Attributes.{ alias; _ } = Attributes.extract ~alias:true attrs in
      let name = Option.value ~default:(String.lowercase txt) alias in
      let schema, ctor_pattern, ctor_value, pattern, value =
        match constructor_arguments_normalize ~loc pcd_args with
        | None ->
          let schema =
            if Option.is_none type_context.attributes.union_key then
              expanded_of_descriptor ~loc
                [%expr String_const [%e Expr.string ~loc name]]
            else expanded_of_descriptor ~loc [%expr Object FieldEnd]
          in
          (schema, None, None, None, None)
        | Some { ctor_pattern; ctor_value; pattern; type_; value; _ } ->
          let p i = Pattern.id ~loc (Fmt.str "f%i" i)
          and v i _ = Expr.id ~loc (Fmt.str "f%i" i) in
          let schema =
            match pcd_args with
            | Pcstr_record fields ->
              _expand_record true ~context ~type_context
                (List.map ~f:expand_field fields)
            | _ -> encode_type ~context ~type_context type_
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
              (if poly then Ppat_variant (txt, pattern)
              else Ppat_construct ({ txt = Lident txt; loc }, ctor_pattern));
            ppat_loc = loc;
            ppat_loc_stack = [];
            ppat_attributes = [];
          }
        and value = Option.value ~default:[%expr ()] value in
        [%expr function [%p pattern] -> Some [%e value] | _ -> None]
      and destruct =
        let value =
          {
            pexp_desc =
              (if poly then Pexp_variant (txt, value)
              else Pexp_construct ({ txt = Lident txt; loc }, ctor_value));
            pexp_loc = loc;
            pexp_loc_stack = [];
            pexp_attributes = [];
          }
        and pattern = Option.value ~default:[%pat? ()] pattern in
        [%expr function [%p pattern] -> [%e value]]
      in
      [%expr
        Schema.Case
          {
            name = [%e Expr.string ~loc name];
            schema = [%e schema_of_expanded ~loc schema];
            encoder = [%e construct];
            decoder = [%e destruct];
          }]
    and key =
      match type_context.attributes.union_key with
      | None -> [%expr None]
      | Some name -> [%expr Some [%e Expr.string ~loc name]]
    in
    expanded_of_descriptor ~loc
      [%expr
        Schema.Union
          { cases = [%e Expr.list ~loc (List.map ~f options)]; key = [%e key] }]

  let expand_abstract_type ~context ~type_context t =
    encode_type ~context ~type_context t

  let close ~configuration:_ ~context ~type_context schema =
    let loc = type_context.loc in
    let schema_id =
      let default =
        let str =
          rename
          @@
          match type_context.name with
          | "t" ->
            String.lowercase
            @@ Option.value
                 ~default:(Code_path.main_module_name context.code_path)
            @@ List.last
            @@ Ppxlib.Code_path.submodule_path context.code_path
          | name -> name
        in
        Expr.string ~loc str
      in
      Option.value ~default type_context.attributes.name
    in
    let type_wrap t =
      let wrap param t =
        match param with
        | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ }, _ ->
          [%type: [%t Type.make ~loc @@ Ptyp_var name] Schema.t -> [%t t]]
        | { ptyp_desc = Ptyp_any; _ }, _ -> [%type: _ -> [%t t]]
        | t, _ ->
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: unsupported type parameter %a" Pprintast.core_type t
      in
      List.fold_right ~f:wrap ~init:t type_context.params
    and params_wrap body =
      let wrap arg exp =
        match arg with
        | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ }, _ ->
          [%expr
            fun [%p pattern_id ~loc @@ schema_name_versioned name] -> [%e exp]]
        | { ptyp_desc = Ptyp_any; _ }, _ -> [%expr fun _ -> [%e exp]]
        | t, _ ->
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: unsupported type parameter %a" Pprintast.core_type t
      in
      List.fold_right ~f:wrap ~init:body type_context.params
    and params_apply f =
      let wrap f arg =
        match arg with
        | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ }, _ ->
          [%expr [%e f] [%e Expr.id ~loc @@ schema_name_versioned name]]
        | { ptyp_desc = Ptyp_any; _ }, _ -> [%expr [%e f] ()]
        | t, _ ->
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: unsupported type parameter %a" Pprintast.core_type t
      in
      List.fold_left ~f:wrap ~init:f type_context.params
    in
    let schema, cache =
      let id =
        let f param exp =
          match param with
          | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ }, _ ->
            [%expr
              Schematic.Schema.name_combine
                [%e Expr.id ~loc (schema_name_versioned name)]
                [%e exp]]
          | { ptyp_desc = Ptyp_any; _ }, _ -> exp
          | t, _ ->
            Ppxlib.Location.raise_errorf ~loc
              "schematic-ppx: unsupported type parameter %a" Pprintast.core_type
              t
        in
        List.fold_right ~init:schema_id ~f type_context.params
      in
      match schema with
      | Components c ->
        let descriptor = params_apply [%expr descriptor] in
        let parametric =
          match type_context.params with
          | [] -> Expr.none ~loc
          | _ ->
            let init =
              [%expr
                Plain { name = [%e schema_id]; descriptor = [%e descriptor] }]
            and f arg parametric =
              match arg with
              | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ }, _ ->
                let schema_name = schema_name_versioned name in
                [%expr
                  Parametric
                    {
                      name = [%e Expr.string ~loc name];
                      parameter = [%e Expr.id ~loc schema_name];
                      next =
                        (fun [%p Pattern.id ~loc schema_name] ->
                          [%e parametric]);
                    }]
              | t, _ ->
                Ppxlib.Location.raise_errorf ~loc
                  "schematic-ppx: unsupported type parameter %a"
                  Pprintast.core_type t
            in
            Expr.some ~loc @@ List.fold_right ~f ~init type_context.params
        in
        let cache = List.is_empty type_context.params in
        ( [%expr
            let descriptor =
              [%e
                if cache then
                  [%expr
                    match
                      Base.Map.find !__schematic_cache __schematic_version
                    with
                    | None ->
                      Schema.fixpoint (fun __schematic_self ->
                          __schematic_cache :=
                            Base.Map.set !__schematic_cache
                              ~key:__schematic_version ~data:__schematic_self;
                          [%e c.descriptor])
                    | Some schema -> schema]
                else params_wrap c.descriptor]
            and id = [%e id] in
            Schema.
              {
                descriptor = [%e descriptor];
                id = Some id;
                parametric = [%e parametric];
              }],
          cache )
      | Expression e ->
        ([%expr { ([%e e]) with id = [%e Expr.some ~loc id] }], false)
    in
    let name, name_versioned =
      (schema_name type_context.name, schema_name_versioned type_context.name)
    in
    let body =
      params_wrap (params_apply [%expr [%e Expr.id ~loc name_versioned] None])
    and body_versioned =
      let body =
        if context.inside then
          [%expr
            fun (__schematic_version : Version.t option) ->
              [%e params_wrap schema]]
        else
          [%expr
            fun (__schematic_version : Schematic.Version.t option) ->
              [%e params_wrap schema]]
      in
      if cache then
        if context.inside then
          [%expr
            let __schematic_cache =
              ref (Base.Map.empty (module Version.Option))
            in
            [%e body]]
        else
          [%expr
            let __schematic_cache =
              ref (Base.Map.empty (module Schematic.Version.Option))
            in
            [%e body]]
      else body
    in
    let pattern, pattern_versioned =
      ( Pattern.make ~loc
          (Ppat_constraint
             ( Pattern.id ~loc name,
               type_wrap
                 [%type:
                   [%t
                     Type.id ~loc
                       ~parameters:(List.map ~f:fst type_context.params)
                       type_context.name]
                   Schema.t] )),
        Pattern.id ~loc name_versioned )
    in
    ( {
        pvb_pat = pattern_versioned;
        pvb_expr = body_versioned;
        pvb_loc = loc;
        pvb_attributes =
          [ Warning.unused_var_strict ~loc; Warning.unused_rec ~loc ];
      },
      {
        pvb_pat = pattern;
        pvb_expr = body;
        pvb_loc = loc;
        pvb_attributes =
          [ Warning.unused_var_strict ~loc; Warning.unused_rec ~loc ];
      } )

  let combine ~loc () expanded =
    let versioned, unversioned = List.unzip expanded in
    [
      { pstr_desc = Pstr_value (Recursive, versioned); pstr_loc = loc };
      { pstr_desc = Pstr_value (Nonrecursive, unversioned); pstr_loc = loc };
    ]
end

module Jsoo_type = struct
  type closed = type_declaration list
  type configuration = { functions : bool }

  type expanded =
    | Type of core_type
    | Object of core_type

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

  and expand_record ~context ~type_context fields =
    let loc = type_context.loc in
    let f Attributes.{ alias; inline; loc; name; type_; _ } =
      let desc =
        match Option.value ~default:Outline inline with
        | Inline -> (
          match type_.ptyp_desc with
          | Ptyp_constr (id, parameters) ->
            Some
              (Oinherit
                 {
                   ptyp_desc =
                     Ptyp_constr
                       ( { txt = suffix_id ~loc "js_fields" id.txt; loc },
                         List.map ~f:(expand_type ~loc ~context) parameters );
                   ptyp_loc = loc;
                   ptyp_loc_stack = [];
                   ptyp_attributes = [];
                 })
          | _ -> None)
        | Inline_try -> None
        | Outline ->
          let t = expand_type ~loc ~context type_ in
          let t = [%type: [%t t] Js_of_ocaml.Js.readonly_prop] in
          Some (Otag ({ loc; txt = Option.value ~default:name alias }, t))
      in
      Option.map
        ~f:(fun pof_desc -> { pof_desc; pof_loc = loc; pof_attributes = [] })
        desc
    in
    Object
      {
        ptyp_desc = Ptyp_object (List.filter_map ~f fields, Closed);
        ptyp_attributes = [];
        ptyp_loc = loc;
        ptyp_loc_stack = [];
      }

  and expand_variant ~context ~type_context options =
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
    Type
      (expand_any_variant ~loc:type_context.loc ~context false
      @@ List.map ~f options)

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
           ~f:(function _, Pcstr_tuple [], _, _ -> false | _ -> true)
           options
    then [%type: Js_of_ocaml.Js.Unsafe.any]
    else
      {
        ptyp_desc = Ptyp_constr ({ txt = Lident "string_js"; loc }, []);
        ptyp_loc = loc;
        ptyp_loc_stack = [];
        ptyp_attributes = [];
      }

  let expand_abstract_type ~context ~type_context t =
    Type (expand_type ~loc:type_context.loc ~context t)

  let close ~configuration:_ ~context:_ ~type_context =
    let loc = type_context.loc in
    function
    | Type manifest ->
      [
        {
          ptype_name = { loc; txt = name "js" type_context.name };
          ptype_params = type_context.params;
          ptype_cstrs = [];
          ptype_kind = Ptype_abstract;
          ptype_private = Public;
          ptype_manifest = Some manifest;
          ptype_loc = type_context.loc;
          ptype_attributes = [];
        };
      ]
    | Object manifest ->
      [
        {
          ptype_name =
            { loc = type_context.loc; txt = name "js_fields" type_context.name };
          ptype_params = type_context.params;
          ptype_cstrs = [];
          ptype_kind = Ptype_abstract;
          ptype_private = Public;
          ptype_manifest = Some manifest;
          ptype_loc = type_context.loc;
          ptype_attributes = [];
        };
        {
          ptype_name =
            { loc = type_context.loc; txt = name "js" type_context.name };
          ptype_params = type_context.params;
          ptype_cstrs = [];
          ptype_kind = Ptype_abstract;
          ptype_private = Public;
          ptype_manifest =
            Some
              {
                ptyp_desc =
                  Ptyp_constr
                    ( {
                        loc;
                        txt = Ldot (Ldot (Lident "Js_of_ocaml", "Js"), "t");
                      },
                      [
                        Type.id ~loc
                          ~parameters:(List.map ~f:fst type_context.params)
                          (name "js_fields" type_context.name);
                      ] );
                ptyp_attributes = [];
                ptyp_loc = loc;
                ptyp_loc_stack = [];
              };
          ptype_loc = type_context.loc;
          ptype_attributes = [];
        };
      ]

  let combine ~loc { functions } items =
    let items = List.concat items in
    let converters { ptype_name = { txt; _ }; ptype_params; _ } =
      match String.chop_suffix ~suffix:"js" txt with
      | None -> None
      | Some prefix ->
        let name s = prefix ^ s in
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
                  Js_of_ocaml.Js_error.raise_
                    (Js_of_ocaml.Js_error.of_error
                       ((fun (type res t0)
                             (t1 :
                               (t0 -> res Js_of_ocaml.Js.t)
                               Js_of_ocaml.Js.constr) (t0 : t0) (_ : unit) :
                             res Js_of_ocaml.Js.t ->
                          Js_of_ocaml.Js.Unsafe.new_obj t1
                            [| Js_of_ocaml.Js.Unsafe.inject t0 |])
                          Js_of_ocaml.Js.error_constr
                          (Js_of_ocaml.Js.string
                          @@ Schematic.Error.decoding_to_string e)
                          ()))]
          in
          parametrize body
        in
        Some
          [%stri
            let [%p fname "to_js"], [%p fname "of_js"], [%p fname "of_js_exn"] =
              let to_js = [%e to_js]
              and of_js = [%e of_js] in
              let of_js_exn = [%e of_js_exn] in
              (to_js, of_js, of_js_exn)]
    and type_ = { pstr_desc = Pstr_type (Recursive, items); pstr_loc = loc } in
    if functions then type_ :: List.filter_map ~f:converters items
    else [ type_ ]
end

module Sqml_binding = struct
  type closed = structure_item list
  type configuration = { name : string }

  type expanded = {
    columns : structure_item list;
    column_explosions : structure_item list;
    columns_list : expression;
    columns_pattern : pattern;
    columns_type : core_type;
    expr_explosions : structure_item list;
    record : expression;
    record_pattern : pattern;
  }

  let expand_record ~context ~type_context (fields : Attributes.field list) =
    let loc = type_context.loc
    and column_name s = "column_" ^ s in
    let column_explosion_of_field (field : Attributes.field) =
      [%stri
        let Sqml.Expression.Column.List.(
              [%p Pattern.id ~loc:field.loc field.name] :: rest) =
          rest]
    and column_of_field (field : Attributes.field) =
      let loc = field.loc in
      let column_type =
        match field.sqml_type with
        | Some t -> t
        | None ->
          [%expr
            Schematic_sqml.type_of_schema
              [%e
                Schema.(
                  schema_of_expanded ~loc:field.loc
                  @@ encode_type ~context ~type_context field.type_)]]
      and primary =
        Expr.bool ~loc (Option.value ~default:false field.sqml_primary)
      in
      [%stri
        let [%p Pattern.id ~loc:field.loc (column_name field.name)] =
          Sqml.Column.make ~primary:[%e primary]
            [%e Expr.string ~loc:field.loc field.name]
            [%e column_type]]
    and column_type_of_field (field : Attributes.field) tail =
      [%type: [%t field.type_] * [%t tail]]
    and expr_explosion_of_field (field : Attributes.field) =
      [%stri
        let Sqml.Expression.List.(
              [%p Pattern.id ~loc:field.loc field.name] :: rest) =
          rest]
    and id_of_field (field : Attributes.field) =
      Expr.id ~loc:field.loc (column_name field.name)
    and pattern_of_field (field : Attributes.field) =
      Pattern.id ~loc:field.loc (column_name field.name)
    and record_field_of_field (field : Attributes.field) =
      ( { txt = Lident field.name; loc = field.loc },
        Expr.id ~loc:field.loc (column_name field.name) )
    and record_pattern_of_field (field : Attributes.field) =
      ( { txt = Lident field.name; loc = field.loc },
        Pattern.id ~loc:field.loc (column_name field.name) )
    in
    {
      columns = List.map ~f:column_of_field fields;
      column_explosions = List.map ~f:column_explosion_of_field fields;
      columns_list = Expr.list ~loc (List.map ~f:id_of_field fields);
      columns_pattern =
        [%pat?
          Acid.Hlist.(
            [%p Pattern.list ~loc (List.map ~f:pattern_of_field fields)])];
      columns_type =
        List.fold_right ~init:[%type: unit] ~f:column_type_of_field fields;
      expr_explosions = List.map ~f:expr_explosion_of_field fields;
      record =
        Expr.make ~loc
          (Pexp_record (List.map ~f:record_field_of_field fields, None));
      record_pattern =
        Pattern.make ~loc
          (Ppat_record (List.map ~f:record_pattern_of_field fields, Closed));
    }

  and expand_variant ~context:_ ~type_context _ =
    Ppxlib.Location.raise_errorf ~loc:type_context.loc
      "schematic-ppx: SQML can only bind record types"

  let expand_abstract_type ~context:_ ~type_context _ =
    Ppxlib.Location.raise_errorf ~loc:type_context.loc
      "schematic-ppx: SQML can only bind record types"

  let close ~configuration:{ name } ~context:_ ~type_context
      {
        column_explosions;
        columns;
        columns_list;
        columns_pattern;
        columns_type;
        expr_explosions;
        record;
        record_pattern;
      } =
    let loc = type_context.loc in
    let meta =
      [%str
        include Sqml.Table.Meta (struct
          let name = [%e Expr.string ~loc name]
          let __schematic_version = None

          include
            [%m
            {
              pmod_desc = Pmod_structure columns;
              pmod_loc = loc;
              pmod_attributes = [];
            }]

          type columns = [%t columns_type]

          let columns = Sqml.Column.List.([%e columns_list])
        end)

        type mapped = [%t Type.id ~loc:type_context.loc type_context.name]

        let map [%p columns_pattern] = [%e record]
        let unmap [%p record_pattern] = Acid.Hlist.([%e columns_list])

        module Import (Table : Sqml.Table.S with type Meta.columns = columns) =
        struct
          include Table

          module Column = struct
            include Table.Column

            include
              [%m
              {
                pmod_desc =
                  Pmod_structure
                    ([%stri let rest = columns] :: column_explosions);
                pmod_loc = loc;
                pmod_attributes = [];
              }]
          end

          include
            [%m
            {
              pmod_desc =
                Pmod_structure
                  ([%stri let rest = Expression.columns] :: expr_explosions);
              pmod_loc = loc;
              pmod_attributes = [];
            }]
        end]
    in
    [
      {
        pstr_desc =
          Pstr_module
            {
              pmb_name = { txt = Some ("Sqml_meta_" ^ name); loc };
              pmb_expr =
                {
                  pmod_desc = Pmod_structure meta;
                  pmod_loc = loc;
                  pmod_attributes = [];
                };
              pmb_loc = loc;
              pmb_attributes = [];
            };
        pstr_loc = loc;
      };
    ]

  let combine ~loc:_ _ items = List.concat items
end

module Expander (Expansion : EXPANSION) = struct
  let expand ~configuration ~context types =
    let f (type_, type_context) =
      let body =
        match type_ with
        | Abstract -> failwith "no manifest on abstract type"
        | Core core ->
          Expansion.expand_abstract_type ~context ~type_context core
        | Record fields -> Expansion.expand_record ~context ~type_context fields
        | Variant options ->
          Expansion.expand_variant ~context ~type_context options
      in
      Expansion.close ~configuration ~context ~type_context body
    in
    let expanded = List.map ~f types in
    Expansion.combine ~loc:context.loc configuration expanded
end

module Jsoo_type_expander = Expander (Jsoo_type)
module Schema_expander = Expander (Schema)
module Sqml_binding_expander = Expander (Sqml_binding)

let expand ~ctxt ~inside (recursive, types) jsoo sqml =
  let loc = Ppxlib.Expansion_context.Deriver.derived_item_loc ctxt in
  let context =
    let recursive = Poly.(recursive = Recursive)
    and code_path = Ppxlib.Expansion_context.Deriver.code_path ctxt in
    let types =
      let f { ptype_name; _ } = ptype_name.txt in
      if recursive then List.map ~f types else []
    in
    { code_path; inside; loc; types; recursive }
  in
  let types =
    let f t =
      let type_context =
        {
          attributes =
            Attributes.extract ~computed:true ~name:true ~union_key:true
              t.ptype_attributes;
          loc = t.ptype_loc;
          name = t.ptype_name.txt;
          params = t.ptype_params;
        }
      in
      let () =
        match (type_context.attributes.union_key, t.ptype_kind) with
        | None, _ | Some _, Ptype_variant _ -> ()
        | _ ->
          Ppxlib.Location.raise_errorf ~loc:t.ptype_loc
            "schematic-ppx: union_key is only valid on variants"
      and () =
        match t.ptype_kind with
        | Ptype_record _ -> ()
        | _ when Map.length type_context.attributes.computed = 0 -> ()
        | _ ->
          Ppxlib.Location.raise_errorf ~loc:t.ptype_loc
            "schematic-ppx: computed is only valid on records"
      in
      let loc = t.ptype_loc in
      let type_ =
        match t.ptype_kind with
        | Ptype_record fields ->
          let fields = List.map ~f:expand_field fields
          and computed =
            let f (name, (value, type_)) =
              Attributes.make_field ~loc:value.pexp_loc ~type_ ~computed:value
                name
            in
            Map.to_sequence type_context.attributes.computed
            |> Sequence.map ~f |> Sequence.to_list
          in
          Record (fields @ computed)
        | Ptype_abstract -> (
          match t.ptype_manifest with
          | Some type_ -> Core type_
          | None -> Abstract)
        | Ptype_open ->
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: unsupported Ptype_open"
        | Ptype_variant options -> Variant options
      in
      (type_, type_context)
    in
    List.map ~f types
  in
  let contents =
    let contents =
      match jsoo with
      | None -> []
      | Some [%expr jsoo] | Some [%expr true] ->
        Jsoo_type_expander.expand ~configuration:{ functions = true } ~context
          types
      | Some [%expr false] ->
        Jsoo_type_expander.expand
          ~configuration:Jsoo_type.{ functions = false }
          ~context types
      | Some exp ->
        Ppxlib.Location.raise_errorf ~loc:exp.pexp_loc
          "schematic-ppx: invalid ~jsoo argument value: %a" Pprintast.expression
          exp
    in
    let contents =
      match sqml with
      | None -> contents
      | Some name ->
        Sqml_binding_expander.expand ~configuration:{ name } ~context types
        @ contents
    in
    Schema_expander.expand ~configuration:() ~context types @ contents
  in
  if Option.is_some jsoo then
    [%str
      open Schematic_jsoo.Schemas

      let _ = fun (_ : int_js) -> ()]
    @ contents
  else if inside then
    [%str
      open Schemas

      let _ = unit_schema]
    @ contents
  else
    [%str
      open Schematic.Schemas

      let _ = unit_schema]
    @ contents

let add inside =
  let str_type_decl =
    let open Deriving.Args in
    let arg =
      let jsoo = arg "jsoo" __
      and sqml = arg "sqml" (estring __) in
      empty +> jsoo +> sqml
    in
    Deriving.Generator.V2.make arg (expand ~inside)
  in
  Deriving.add ~str_type_decl "schema"

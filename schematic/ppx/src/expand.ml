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
end

module Warning = struct
  let make ~loc n =
    let module Ast = (val Ast_builder.make loc) in
    Ast.attribute ~name:{ txt = "warning"; loc }
      ~payload:
        (PStr [ { pstr_desc = Pstr_eval (Ast.estring n, []); pstr_loc = loc } ])

  let unused_var_strict ~loc = make ~loc "-unused-var-strict"
  let unused_rec ~loc = make ~loc "-unused-rec-flag"
end

let param_prefix = "__schematic_param_"

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

let constructor_arguments_normalize ~loc =
  let module Ast = (val Ast_builder.make loc) in
  function
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
    let type_ = Ast.ptyp_tuple types
    and pattern f =
      let f i _ = f i in
      Ast.ppat_tuple (List.mapi ~f types)
    and value f = Ast.pexp_tuple (List.mapi ~f types) in
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
      Ast.ppat_record
        (List.mapi
           ~f:(fun i v -> ({ txt = Lident v.pld_name.txt; loc }, f i))
           fields)
        Closed
    and ctor_value f =
      Ast.pexp_record
        (List.mapi
           ~f:(fun i { pld_name; pld_type; pld_loc = loc; _ } ->
             ({ txt = Lident pld_name.txt; loc }, f i pld_type))
           fields)
        None
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
          Ast.ppat_tuple (List.mapi ~f types)
        and value f = Ast.pexp_tuple (List.mapi ~f types) in
        (Ast.ptyp_tuple types, pattern, value)
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

let expect ~loc pattern ast = Ast_pattern.parse pattern loc ast Fn.id

let rename name =
  let len = String.length name in
  if name.[len - 1] = '_' then String.sub name ~pos:0 ~len:(len - 1) else name

let name suffix s =
  let s = rename s in
  if s = "t" then suffix else s ^ "_" ^ suffix

let suffix_id ~loc ?(prefix = "") suffix = function
  | Lident n -> Lident (prefix ^ name suffix n)
  | Ldot (q, n) -> Ldot (q, prefix ^ name suffix n)
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
  type 'a versioned =
    ('a * (Semver.t * [ `Eq | `Gt | `Gte | `Lt | `Lte ]) option) list

  type field = {
    alias : string versioned; [@default []]
    computed : expression option;
    inline : inline option;
    loc : location;
    name : string; [@main]
    schema : expression versioned; [@default []]
    specs : specs; [@default specs_empty]
    sqml_foreign : expression list;
    sqml_primary : bool option;
    sqml_type : expression option;
    type_ : core_type;
  }
  [@@deriving make]

  type requirement =
    | Computed
    | Computed_optional
    | Default_opt of expression
    | Optional
    | Required

  let requirement { computed; loc; specs = { default; _ }; type_; _ } =
    match type_ with
    | [%type: [%t? t] option] when Option.is_none computed -> (Optional, t)
    | [%type: [%t? t] option] -> (Computed_optional, t)
    | t -> (
      match (default, computed) with
      | Some default, _ ->
        ( Default_opt
            [%expr
              match [%e Source.value default] with
              | Some default -> Default default
              | None -> Required],
          t )
      | None, Some _ -> (Computed, t)
      | None, None -> (Required, t))

  type t = {
    alias : string option;
    computed :
      (string, expression * core_type, String.comparator_witness) Map.t;
    gadt_variant : string option;
    name : expression option;
    schema : expression option;
    union_key : string option;
  }

  let field_of_computed ~loc (name, (value, type_)) =
    make_field ~loc:value.pexp_loc ~type_ ~computed:[%expr Some [%e value]] name

  let fields_of_computed ~loc computed =
    Map.to_sequence computed
    |> Sequence.map ~f:(field_of_computed ~loc)
    |> Sequence.to_list

  let check_unknown_attribute ~loc name =
    match String.chop_prefix ~prefix:"schematic." name with
    | Some name ->
      Ppxlib.Location.raise_errorf ~loc
        "schematic-ppx: unknown schematic annotation %s" name
    | None -> ()

  let rec extract ?(alias = false) ?(computed = false) ?(gadt_variant = false)
      ?(name = false) ?(schema = false) ?(union_key = false) =
    let merge name ~loc old v =
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
                        (pair (loc (lident __)) (pexp_constraint __ __ |> pack2)
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
      | "schematic.gadt" when gadt_variant ->
        {
          attrs with
          gadt_variant =
            merge "gadt" ~loc attrs.gadt_variant
            @@ expect ~loc
                 Ast_pattern.(single_expr_payload (pexp_ident (lident __)))
                 attr_payload;
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
      | "schematic.schema" when schema ->
        {
          attrs with
          schema =
            merge "schema" ~loc attrs.schema
            @@ expect ~loc Ast_pattern.(single_expr_payload __) attr_payload;
        }
      | "schematic.union_key" when union_key ->
        {
          attrs with
          union_key =
            merge "union_key" ~loc attrs.union_key
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
        gadt_variant = None;
        schema = None;
        name = None;
        union_key = None;
      }
end

type context = {
  code_path : Ppxlib.Code_path.t;
  fixpoint : bool;
  gadt_variant : string option;
  inside : bool;
  loc : location;
  recursive : bool;
  types : string list;
  version : Semver.t option;
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

let versioned versioned unversioned =
  let open Ast_pattern in
  let versioned =
    let+ op, (schema, version) =
      let version v =
        let f context loc pl cont =
          parse (estring __) loc pl (fun s ->
              match Semver.of_string s with
              | None -> Location.raise_errorf ~loc "invalid semantic version"
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
                Location.raise_errorf ~loc "invalid version operator: %s" op)
        in
        of_func f
      in
      pexp_apply (op __)
        (pair nolabel versioned ^:: pair nolabel (version __) ^:: nil |> pack2)
      |> pack2
    in
    (schema, Some (version, op))
  and unversioned =
    let+ expr = unversioned in
    (expr, None)
  in
  single_expr_payload (versioned ||| unversioned)

let version_match ~loc ~f init options =
  let f next = function
    | schema, None -> f schema
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
              Schematic.Version.equal l r || Schematic.Version.greater_than l r]
        | `Lt -> [%expr Schematic.Version.less_than]
        | `Lte ->
          [%expr
            fun l r ->
              Schematic.Version.equal l r || Schematic.Version.less_than l r]
      in
      [%expr
        match __schematic_schema_version with
        | Some version when [%e predicate] version [%e version] -> [%e f schema]
        | _ -> [%e next]]
  in
  List.fold ~init ~f options

let expand_field _
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
    | "default" -> default Source.generic true false
    | "schematic.as" ->
      let pattern = Ast_pattern.(versioned (estring __) (estring __)) in
      { attrs with alias = expect ~loc pattern attr_payload :: attrs.alias }
    | "schematic.default" -> default Source.schematic true false
    | "schematic.default_opt" -> default Source.schematic false false
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
    | "schematic.schema" ->
      let pattern = Ast_pattern.(versioned __ __) in
      { attrs with schema = expect ~loc pattern attr_payload :: attrs.schema }
    | "schematic.sqml.foreign_key" ->
      Ast_pattern.parse
        Ast_pattern.(single_expr_payload __)
        loc attr_payload
        (fun pl ->
          {
            attrs with
            sqml_foreign =
              [%expr
                let open Sqml.Table.Column.Foreign_key in
                [%e pl]]
              :: attrs.sqml_foreign;
          })
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

  let schema_anonymous_name_versioned i =
    schema_name_versioned @@ Fmt.str "%i_any" i

  let schema_id_versioned ?prefix ~loc =
    suffix_id ~loc ?prefix "schema_versioned"

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

  let rec encode_type ~context ~type_context core_type =
    let Attributes.{ schema; _ } =
      Attributes.extract ~schema:true core_type.ptyp_attributes
    in
    let loc = type_context.loc in
    match schema with
    | Some schema -> Expression [%expr [%e schema] __schematic_version]
    | None -> (
      let module Ast = (val Ast_builder.make loc) in
      match core_type with
      (* Mapping *)
      | [%type: (string * [%t? t]) list] ->
        expanded_of_descriptor ~loc
          [%expr
            Schema.Mapping
              [%e
                schema_of_expanded ~loc @@ encode_type ~context ~type_context t]]
      (* Plain type *)
      | { ptyp_desc = Ptyp_constr ({ txt; loc }, []); _ } ->
        Expression
          [%expr
            [%e Ast.pexp_ident @@ { loc; txt = schema_id_versioned ~loc txt }]
              __schematic_version]
      (* Lists *)
      | [%type: [%t? t] list] ->
        expanded_of_descriptor ~loc
          [%expr
            Schema.List
              [%e
                schema_of_expanded ~loc @@ encode_type ~context ~type_context t]]
      (* Parametric type *)
      | { ptyp_desc = Ptyp_constr ({ txt; loc }, args); _ } ->
        let schema_f =
          Ast.pexp_ident { loc; txt = schema_id_versioned ~loc txt }
        and f t =
          ( Nolabel,
            schema_of_expanded ~loc @@ encode_type ~context ~type_context t )
        in
        Expression
          (Ast.pexp_apply schema_f
             ((Nolabel, [%expr __schematic_version]) :: List.map ~f args))
      (* Tuple *)
      | { ptyp_desc = Ptyp_tuple types; _ } ->
        let field_name i { ptyp_loc = loc; _ } = (loc, "e" ^ Int.to_string i)
        and field_value i { ptyp_loc = loc; _ } =
          Ast.pexp_ident { loc; txt = Lident ("e" ^ Int.to_string i) }
        and constructed =
          let f i _ =
            Ast.pexp_ident { loc; txt = Lident ("e" ^ Int.to_string i) }
          in
          Ast.pexp_tuple (List.mapi ~f types)
        and deconstructed =
          let f i _ = Ast.ppat_var { loc; txt = "e" ^ Int.to_string i } in
          Ast.ppat_tuple (List.mapi ~f types)
        in
        let body =
          combinator ~loc types field_name field_value constructed deconstructed
            (tuple_descriptor ~context ~type_context types)
        in
        expanded_of_descriptor ~loc body
      (* Type variable *)
      | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ } ->
        Expression
          (Ast.pexp_ident
             {
               loc;
               txt = schema_id_versioned ~loc ~prefix:param_prefix (Lident name);
             })
      (* Polymorphic variant *)
      | { ptyp_desc = Ptyp_variant (fields, _, _); _ } ->
        expand_polymorphic_variant ~context ~type_context fields
      (* Unsupported *)
      | core_type ->
        Ppxlib.Location.raise_errorf ~loc "schematic-ppx: unsupported type %a"
          Pprintast.core_type core_type)

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
    let module Ast = (val Ast_builder.make loc) in
    let construct =
      let pattern =
        let pattern i f =
          let loc, txt = field_name i f in
          Ast.ppat_var { loc; txt }
        in
        let f i tl field =
          let pattern = pattern i field in
          [%pat? [%p pattern], [%p tl]]
        in
        List.fold_righti ~f ~init:[%pat? ()] fields
      in
      Ast.pexp_fun Nolabel None pattern [%expr Result.Ok [%e constructed]]
    and deconstruct =
      let value =
        let expr i f = field_value i f in
        let f i tl field =
          let expr = expr i field in
          [%expr [%e expr], [%e tl]]
        in
        List.fold_righti ~f ~init:[%expr ()] fields
      in
      Ast.pexp_fun Nolabel None deconstructed value
    in
    [%expr
      Schema.Map
        {
          decode = [%e construct];
          encode = [%e deconstruct];
          descriptor = [%e obj];
        }]

  and expand_record ~context ~type_context fields =
    _expand_record false ~context ~type_context fields

  and _expand_record as_tuple ~context ~type_context
      (fields : Attributes.field list) =
    let module Ast = (val Ast_builder.make type_context.loc) in
    let loc = type_context.loc in
    let type_ =
      (* This type is used to explicitely disambiguate mapped values in case of
         record field name conflicts, otherwise a schema can end up manipulating
         a completely different type. *)
      Ast.ptyp_constr
        (Ast.Located.lident type_context.name)
        (List.map ~f:fst type_context.params)
    in
    let fields_schema =
      let init = [%expr FieldEnd] in
      let f
          Attributes.(
            { alias; inline; loc; name; schema; specs; _ } as attributes) rest =
        let requirement, type_ = Attributes.requirement attributes in
        let requirement =
          match requirement with
          | Computed -> [%expr Computed]
          | Computed_optional -> [%expr Computed_optional]
          | Default_opt e -> e
          | Optional -> [%expr Optional]
          | Required -> [%expr Required]
        in
        let alias =
          version_match ~loc ~f:Ast.estring (Ast.estring (rename name)) alias
        and schema =
          let schema =
            version_match ~loc
              ~f:(fun schema -> [%expr [%e schema] __schematic_version])
              (schema_of_expanded ~loc
              @@ encode_type ~context ~type_context type_)
              schema
          in
          match Option.value ~default:Outline inline with
          | Inline -> [%expr Inline [%e schema]]
          | Inline_try -> [%expr Inline_try [%e schema]]
          | Outline -> [%expr Outline [%e schema]]
        in
        [%expr
          Field
            {
              field =
                {
                  description =
                    [%e
                      expr_option ~loc Ast_builder.Default.estring
                        specs.description];
                  examples = [%e Ast.elist specs.examples];
                  maximum =
                    [%e expr_option ~loc Ast_builder.Default.eint specs.maximum];
                  minimum =
                    [%e expr_option ~loc Ast_builder.Default.eint specs.minimum];
                  name = [%e alias];
                  omit = [%e Ast.ebool specs.omit];
                  requirement = [%e requirement];
                  schema = [%e schema];
                  title = None;
                };
              rest = [%e rest];
            }]
      in
      List.fold_right ~f ~init fields
    and field_name _ (field : Attributes.field) = (field.loc, field.name)
    and field_value _ (field : Attributes.field) =
      match field.computed with
      | None -> Ast.pexp_ident { loc; txt = Lident field.name }
      | Some computed -> computed
    and constructed =
      let f (field : Attributes.field) =
        let value = Ast.pexp_ident { txt = Lident field.name; loc } in
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
          Ast.pexp_tuple
            (List.filter_map ~f:(fun x -> f x |> Option.map ~f:snd) fields)
      else
        let constructed = Ast.pexp_record (List.filter_map ~f fields) None in
        [%expr ([%e constructed] : [%t type_])]
    and deconstructed =
      let f (field : Attributes.field) =
        match field.computed with
        | Some _ -> None
        | None ->
          Some
            ( { txt = Lident field.name; loc },
              Ast.ppat_var { txt = field.name; loc } )
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
          Ast.ppat_tuple
            (List.filter_map ~f:(fun x -> f x |> Option.map ~f:snd) fields)
      else
        let deconstructed =
          Ast.ppat_record (List.filter_map ~f fields) Closed
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
    let module Ast = (val Ast_builder.make type_context.loc) in
    let loc = type_context.loc in
    let options =
      let f (txt, pcd_args, loc, attrs) =
        let (Attributes.{ gadt_variant; _ } as attrs) =
          Attributes.extract ~alias:true
            ~computed:
              (match pcd_args with Pcstr_record _ -> true | _ -> false)
            ~gadt_variant:(Option.is_some context.gadt_variant)
            attrs
        in
        match (context.gadt_variant, gadt_variant) with
        | Some _, None -> None
        | Some expected, Some effective when String.(expected <> effective) ->
          None
        | _ -> Some (txt, pcd_args, loc, attrs)
      in
      List.filter_map ~f options
    in
    let f (txt, pcd_args, loc, Attributes.{ alias; computed; _ }) =
      let module Ast = (val Ast_builder.make loc) in
      let name = Option.value ~default:(String.lowercase txt) alias in
      let schema, ctor_pattern, ctor_value, pattern, value =
        match constructor_arguments_normalize ~loc pcd_args with
        | None ->
          let schema =
            if Option.is_none type_context.attributes.union_key then
              expanded_of_descriptor ~loc
                [%expr String_const [%e Ast.estring name]]
            else expanded_of_descriptor ~loc [%expr Object FieldEnd]
          in
          (schema, None, None, None, None)
        | Some { ctor_pattern; ctor_value; pattern; type_; value; _ } ->
          let p i = Ast.pvar (Fmt.str "f%i" i)
          and v i _ = Ast.evar (Fmt.str "f%i" i) in
          let schema =
            match pcd_args with
            | Pcstr_record fields ->
              let computed_fields =
                Attributes.fields_of_computed ~loc computed
              in
              _expand_record true ~context ~type_context
                (List.map ~f:(expand_field context) fields @ computed_fields)
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
          if poly then Ast.ppat_variant txt pattern
          else Ast.ppat_construct { txt = Lident txt; loc } ctor_pattern
        and value = Option.value ~default:[%expr ()] value in
        if List.length options > 1 then
          [%expr function [%p pattern] -> Some [%e value] | _ -> None]
        else [%expr function [%p pattern] -> Some [%e value]]
      and destruct =
        let value =
          if poly then Ast.pexp_variant txt value
          else Ast.pexp_construct { txt = Lident txt; loc } ctor_value
        and pattern = Option.value ~default:[%pat? ()] pattern in
        [%expr function [%p pattern] -> [%e value]]
      in
      Some
        [%expr
          Schema.Case
            {
              name = [%e Ast.estring name];
              schema = [%e schema_of_expanded ~loc schema];
              encoder = [%e construct];
              decoder = [%e destruct];
            }]
    and key =
      match type_context.attributes.union_key with
      | None -> [%expr None]
      | Some name -> [%expr Some [%e Ast.estring name]]
    in
    expanded_of_descriptor ~loc
      [%expr
        Schema.Union
          {
            cases = [%e Ast.elist (List.filter_map ~f options)];
            key = [%e key];
          }]

  let expand_abstract_type ~context ~type_context t =
    encode_type ~context ~type_context t

  let close ~configuration:_ ~context ~type_context schema =
    let module Ast = (val Ast_builder.make type_context.loc) in
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
        Ast.estring str
      in
      Option.value ~default type_context.attributes.name
    in
    let type_wrap t =
      let wrap param t =
        match param with
        | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ }, _ ->
          [%type: [%t Ast.ptyp_var name] Schema.t -> [%t t]]
        | { ptyp_desc = Ptyp_any; ptyp_loc = loc; _ }, _ -> [%type: _ -> [%t t]]
        | t, _ ->
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: unsupported type parameter A %a" Pprintast.core_type
            t
      in
      List.fold_right ~f:wrap ~init:t type_context.params
    and params_wrap body =
      let wrap i exp arg =
        match arg with
        | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ }, _ ->
          [%expr
            fun [%p
                  Ast.ppat_var
                    { txt = param_prefix ^ schema_name_versioned name; loc }] ->
              [%e exp]]
        | { ptyp_desc = Ptyp_any; ptyp_loc = loc; _ }, _ ->
          [%expr
            fun [%p
                  Ast.ppat_var
                    {
                      txt = param_prefix ^ schema_anonymous_name_versioned i;
                      loc;
                    }] ->
              [%e exp]]
        | t, _ ->
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: unsupported type parameter B %a" Pprintast.core_type
            t
      in
      type_context.params |> List.rev |> List.foldi ~f:wrap ~init:body
    and params_apply f =
      let wrap i f arg =
        match arg with
        | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ }, _ ->
          [%expr
            [%e f] [%e Ast.evar @@ param_prefix ^ schema_name_versioned name]]
        | { ptyp_desc = Ptyp_any; ptyp_loc = loc; _ }, _ ->
          [%expr
            [%e f]
              [%e Ast.evar @@ param_prefix ^ schema_anonymous_name_versioned i]]
        | t, _ ->
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: unsupported type parameter C %a" Pprintast.core_type
            t
      in
      List.foldi ~f:wrap ~init:f type_context.params
    in
    let schema, cache =
      let id =
        let f param exp =
          match param with
          | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ }, _ ->
            let combine =
              if context.inside then [%expr Schema.name_combine]
              else [%expr Schematic.Schema.name_combine]
            in
            [%expr
              [%e combine]
                [%e Ast.evar (param_prefix ^ schema_name_versioned name)]
                [%e exp]]
          | [%type: _], _ -> exp
          | t, _ ->
            Ppxlib.Location.raise_errorf ~loc
              "schematic-ppx: unsupported type parameter D %a"
              Pprintast.core_type t
        in
        List.fold_right ~init:schema_id ~f type_context.params
      in
      match schema with
      | Components c ->
        let descriptor = params_apply [%expr descriptor] in
        let parametric =
          match type_context.params with
          | [] -> [%expr None]
          | _ ->
            let init =
              [%expr
                Plain { name = [%e schema_id]; descriptor = [%e descriptor] }]
            and f i parametric arg =
              match arg with
              | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ }, _ ->
                let schema_name = schema_name_versioned name in
                [%expr
                  Parametric
                    {
                      name = [%e Ast.estring name];
                      parameter = [%e Ast.evar @@ param_prefix ^ schema_name];
                      next =
                        (fun [%p Ast.pvar @@ param_prefix ^ schema_name] ->
                          [%e parametric]);
                    }]
              | { ptyp_desc = Ptyp_any; ptyp_loc = loc; _ }, _ ->
                let schema_name = schema_anonymous_name_versioned i in
                [%expr
                  Parametric
                    {
                      name = [%e Ast.estring (Fmt.str "anonymous_%i" i)];
                      parameter = [%e Ast.evar @@ param_prefix ^ schema_name];
                      next =
                        (fun [%p Ast.pvar @@ param_prefix ^ schema_name] ->
                          [%e parametric]);
                    }]
              | t, _ ->
                Ppxlib.Location.raise_errorf ~loc
                  "schematic-ppx: unsupported type parameter E %a"
                  Pprintast.core_type t
            in
            [%expr
              Some [%e type_context.params |> List.rev |> List.foldi ~f ~init]]
        in
        let cache = context.fixpoint || List.is_empty type_context.params in
        ( [%expr
            let descriptor =
              [%e
                if cache then
                  [%expr
                    match
                      Base.Map.find !__schematic_cache
                        __schematic_schema_version
                    with
                    | None ->
                      [%e
                        params_wrap
                          [%expr
                            Schema.fixpoint (fun __schematic_self ->
                                __schematic_cache :=
                                  Base.Map.set !__schematic_cache
                                    ~key:__schematic_schema_version
                                    ~data:__schematic_self;
                                [%e c.descriptor])]]
                    | Some schema -> [%e params_wrap [%expr schema]]]
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
        let e =
          if context.inside then
            [%expr { ([%e e]) with Schema.id = Some [%e id] }]
          else [%expr { ([%e e]) with Schematic.Schema.id = Some [%e id] }]
        in
        (e, false)
    in
    let name, name_versioned =
      let name =
        match context.gadt_variant with
        | None -> type_context.name
        | Some variant -> Fmt.str "%s_%s" variant type_context.name
      in
      (schema_name name, schema_name_versioned name)
    in
    let body =
      params_wrap
        (params_apply [%expr [%e Ast.evar name_versioned] (fun _ -> None)])
    and body_versioned =
      let body =
        if context.inside then
          [%expr
            fun (__schematic_version : Schema.version) ->
              let __schematic_schema_version =
                __schematic_version Schemas_library.v
              in
              [%e params_wrap schema]]
        else
          [%expr
            fun (__schematic_version : Schematic.Schema.version) ->
              let __schematic_schema_version =
                __schematic_version Schemas_library.v
              in
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
      ( Ast.ppat_constraint (Ast.pvar name)
          (type_wrap
             [%type:
               [%t
                 Ast.ptyp_constr
                   (Ast.Located.lident type_context.name)
                   (List.map ~f:fst type_context.params)]
               Schema.t]),
        Ast.pvar name_versioned )
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

  let js_escape_keyword word =
    if Keyword.is_keyword word then word ^ "_" else word

  type expanded =
    | Type of core_type
    | Object of core_type

  let expand_ident = function
    | Lident n -> Lident (name "js" n)
    | Ldot (ident, m) -> Ldot (ident, name "js" m)
    | Lapply _ -> failwith "Lapply"

  let rec expand_type ~loc ~context ~type_context t =
    match t.ptyp_desc with
    | Ptyp_constr (name, args) ->
      {
        t with
        ptyp_desc =
          Ptyp_constr
            ( { name with txt = expand_ident name.txt },
              List.map ~f:(expand_type ~loc ~context ~type_context) args );
      }
    (* Polymorphic variant *)
    | Ptyp_variant (fields, _, _) ->
      expand_polymorphic_variant ~loc ~context ~type_context fields
    (* I don't know of any way to properly type this, so any it is *)
    | Ptyp_tuple _ -> [%type: Js_of_ocaml.Js.Unsafe.any]
    (* Type variable *)
    | Ptyp_var _ -> t
    | _ ->
      Ppxlib.Location.raise_errorf ~loc:t.ptyp_loc
        "schematic-ppx: unsupported type %a" Pprintast.core_type t

  and expand_record ~context ~type_context fields =
    let loc = type_context.loc in
    let module Ast = (val Ast_builder.make loc) in
    let f Attributes.({ alias; inline; loc; name; type_; _ } as attributes) =
      let desc =
        match Option.value ~default:Outline inline with
        | Inline -> (
          match type_.ptyp_desc with
          | Ptyp_constr (id, parameters) ->
            Some
              (Oinherit
                 (Ast.ptyp_constr
                    { txt = suffix_id ~loc "js_fields" id.txt; loc }
                    (List.map
                       ~f:(expand_type ~loc ~context ~type_context)
                       parameters)))
          | _ -> None)
        | Inline_try -> None
        | Outline ->
          let t =
            match context.version with
            | Some { major = 0; minor = 13; patch; _ } when patch < 1 ->
              [%type:
                [%t expand_type ~loc ~context ~type_context type_]
                Js_of_ocaml.Js.readonly_prop]
            | _ -> (
              match Attributes.requirement attributes with
              | (Computed | Required), type_ ->
                [%type:
                  [%t expand_type ~loc ~context ~type_context type_]
                  Js_of_ocaml.Js.readonly_prop]
              | (Computed_optional | Default_opt _ | Optional), type_ ->
                [%type:
                  [%t expand_type ~loc ~context ~type_context type_]
                  Js_of_ocaml.Js.Optdef.t
                  Js_of_ocaml.Js.readonly_prop])
          and alias =
            List.find_map
              ~f:(function alias, None -> Some alias | _ -> None)
              alias
          in
          Some (Otag ({ loc; txt = Option.value ~default:name alias }, t))
      in
      Option.map
        ~f:(fun pof_desc -> { pof_desc; pof_loc = loc; pof_attributes = [] })
        desc
    in
    Object (Ast.ptyp_object (List.filter_map ~f fields) Closed)

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
      (expand_any_variant ~loc:type_context.loc ~context ~type_context false
      @@ List.map ~f options)

  and expand_polymorphic_variant ~loc ~context ~type_context options =
    let f { prf_desc; prf_loc; prf_attributes } =
      match prf_desc with
      | Rtag ({ txt; _ }, _, types) ->
        (txt, Pcstr_tuple types, prf_loc, prf_attributes)
      | Rinherit { ptyp_loc = loc; _ } ->
        Ppxlib.Location.raise_errorf ~loc
          "schematic-ppx: inheriting polymorphic variants not supported"
    in
    expand_any_variant ~loc ~context ~type_context true @@ List.map ~f options

  and expand_any_variant ~loc ~context ~type_context _ options =
    let module Ast = (val Ast_builder.make loc) in
    if
      Option.is_some
      @@ List.find
           ~f:(function _, Pcstr_tuple [], _, _ -> false | _ -> true)
           options
    then
      match (context.version, type_context.attributes.union_key) with
      | _, None | Some { major = 0; minor = 13; patch = 0; _ }, _ ->
        [%type: Js_of_ocaml.Js.Unsafe.any]
      | _, Some key ->
        let fields =
          let string_prop =
            [%type:
              Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t
              Js_of_ocaml.Js.readonly_prop]
          in
          Ast.ptyp_object
            [ Ast.otag { txt = js_escape_keyword key; loc } string_prop ]
            Closed
        in
        [%type: [%t fields] Js_of_ocaml.Js.t]
    else [%type: string_js]

  let expand_abstract_type ~context ~type_context t =
    Type (expand_type ~loc:type_context.loc ~context ~type_context t)

  let close ~configuration:_ ~context:_ ~type_context =
    let module Ast = (val Ast_builder.make type_context.loc) in
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
              (Ast.ptyp_constr
                 { loc; txt = Ldot (Ldot (Lident "Js_of_ocaml", "Js"), "t") }
                 [
                   Ast.ptyp_constr
                     (Ast.Located.lident (name "js_fields" type_context.name))
                     (List.map ~f:fst type_context.params);
                 ]);
          ptype_loc = type_context.loc;
          ptype_attributes = [];
        };
      ]

  let combine ~loc { functions } items =
    let module Ast = (val Ast_builder.make loc) in
    let items = List.concat items in
    let converters { ptype_name = { txt; _ }; ptype_params; _ } =
      match String.chop_suffix ~suffix:"js" txt with
      | None -> None
      | Some prefix ->
        let name s = prefix ^ s in
        let fname n = Ast.ppat_var { txt = name n; loc }
        and schema = Ast.pexp_ident { txt = Lident (name "schema"); loc }
        and js =
          Ast.ptyp_constr
            { txt = Lident (name "js"); loc }
            (List.map ~f:(fun _ -> [%type: _]) ptype_params)
        in
        let apply, parametrize =
          let params =
            let f (t, _) =
              match t with
              | [%type: _] -> None
              | { ptyp_desc = Ptyp_var n; _ } ->
                Some (loc, Fmt.str "%s_schema" n)
              | _ ->
                Ppxlib.Location.raise_errorf ~loc
                  "schematic-ppx: unsupported type parameter"
            in
            List.filter_map ~f ptype_params
          in
          let apply e =
            let args =
              let f (loc, name) = Ast_builder.Default.evar ~loc name in
              List.map ~f params
            in
            Ast.eapply e args
          and parametrize e =
            let f (loc, name) f =
              let pattern = Ast.pvar name in
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
    let module Ast = (val Ast_builder.make type_context.loc) in
    let loc = type_context.loc
    and column_name s = "column_" ^ s in
    let column_explosion_of_field (field : Attributes.field) =
      [%stri
        let Sqml.Expression.Column.List.(
              [%p Ast_builder.Default.pvar ~loc:field.loc field.name] :: rest) =
          rest]
    and column_of_field (field : Attributes.field) =
      let module Ast = (val Ast_builder.make field.loc) in
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
      and fk =
        match field.sqml_foreign with
        | [] -> [%expr None]
        | keys -> [%expr Some [%e Ast.elist keys]]
      and primary =
        Ast.ebool (Option.value ~default:false field.sqml_primary)
      in
      [%stri
        let [%p Ast.pvar (column_name field.name)] =
          Sqml.Column.make ~primary:[%e primary] ?foreign_keys:[%e fk]
            [%e Ast.estring field.name] [%e column_type]]
    and column_type_of_field (field : Attributes.field) tail =
      [%type: [%t field.type_] * [%t tail]]
    and expr_explosion_of_field (field : Attributes.field) =
      [%stri
        let Sqml.Expression.List.(
              [%p Ast_builder.Default.pvar ~loc:field.loc field.name] :: rest) =
          rest]
    and id_of_field (field : Attributes.field) =
      Ast_builder.Default.evar ~loc:field.loc (column_name field.name)
    and pattern_of_field (field : Attributes.field) =
      Ast_builder.Default.pvar ~loc:field.loc (column_name field.name)
    and record_field_of_field (field : Attributes.field) =
      ( { txt = Lident field.name; loc = field.loc },
        Ast_builder.Default.evar ~loc:field.loc (column_name field.name) )
    and record_pattern_of_field (field : Attributes.field) =
      ( { txt = Lident field.name; loc = field.loc },
        Ast_builder.Default.pvar ~loc:field.loc (column_name field.name) )
    in
    {
      columns = List.map ~f:column_of_field fields;
      column_explosions = List.map ~f:column_explosion_of_field fields;
      columns_list = Ast.elist (List.map ~f:id_of_field fields);
      columns_pattern =
        [%pat?
          Acid.Hlist.([%p Ast.plist (List.map ~f:pattern_of_field fields)])];
      columns_type =
        List.fold_right ~init:[%type: unit] ~f:column_type_of_field fields;
      expr_explosions = List.map ~f:expr_explosion_of_field fields;
      record = Ast.pexp_record (List.map ~f:record_field_of_field fields) None;
      record_pattern =
        Ast.ppat_record (List.map ~f:record_pattern_of_field fields) Closed;
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
    let module Ast = (val Ast_builder.make type_context.loc) in
    let loc = type_context.loc in
    let meta =
      [%str
        include Sqml.Table.Meta (struct
          let name = [%e Ast.estring name]
          let __schematic_version _ = None

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

        type mapped =
          [%t Ast.ptyp_constr (Ast.Located.lident type_context.name) []]

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

let expand ~ctxt ~inside ~version (recursive, types) fixpoint gadt jsoo sqml =
  let loc = Ppxlib.Expansion_context.Deriver.derived_item_loc ctxt in
  let jsoo =
    match jsoo with
    | None -> None
    | Some [%expr jsoo] | Some [%expr true] -> Some true
    | Some [%expr false] -> Some false
    | Some exp ->
      Ppxlib.Location.raise_errorf ~loc:exp.pexp_loc
        "schematic-ppx: invalid ~jsoo argument value: %a" Pprintast.expression
        exp
  in
  let context =
    let recursive = Poly.(recursive = Recursive)
    and code_path = Ppxlib.Expansion_context.Deriver.code_path ctxt in
    let types =
      let f { ptype_name; _ } = ptype_name.txt in
      if recursive then List.map ~f types else []
    in
    {
      code_path;
      fixpoint;
      gadt_variant = None;
      inside;
      loc;
      types;
      recursive;
      version = !version;
    }
  in
  let expand context =
    let types =
      let f t =
        let type_context =
          let computed =
            match t.ptype_kind with Ptype_record _ -> true | _ -> false
          and union_key =
            match (t.ptype_kind, t.ptype_manifest) with
            | Ptype_variant _, _
            | Ptype_abstract, Some { ptyp_desc = Ptyp_variant _; _ } ->
              true
            | _ -> false
          in
          {
            attributes =
              Attributes.extract ~computed ~name:true ~union_key
                t.ptype_attributes;
            loc = t.ptype_loc;
            name = t.ptype_name.txt;
            params = t.ptype_params;
          }
        in
        let loc = t.ptype_loc in
        let type_ =
          match t.ptype_kind with
          | Ptype_record fields ->
            let fields = List.map ~f:(expand_field context) fields
            and computed =
              Attributes.fields_of_computed ~loc
                type_context.attributes.computed
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
        | Some true ->
          Jsoo_type_expander.expand ~configuration:{ functions = true } ~context
            types
        | Some false ->
          Jsoo_type_expander.expand
            ~configuration:Jsoo_type.{ functions = false }
            ~context types
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
  in
  match gadt with
  | Some variants ->
    let f variant = expand { context with gadt_variant = Some variant } in
    List.map ~f variants |> List.concat
  | None -> expand context

let expand_sig ~ctxt (_recursive, types) =
  let loc = Ppxlib.Expansion_context.Deriver.derived_item_loc ctxt in
  let module Ast = (val Ast_builder.make loc) in
  let f { ptype_name = { txt; loc }; _ } =
    let type_ = Ast.ptyp_constr { txt = Longident.Lident txt; loc } [] in
    [
      Ast.(
        psig_value
        @@ value_description
             ~name:{ txt = name "schema_versioned" txt; loc }
             ~type_:
               [%type:
                 Schematic.Schema.version -> [%t type_] Schematic.Schema.t]
             ~prim:[]);
      Ast.(
        psig_value
        @@ value_description
             ~name:{ txt = name "schema" txt; loc }
             ~type_:[%type: [%t type_] Schematic.Schema.t] ~prim:[]);
    ]
  in
  List.map ~f types |> List.concat

let add ~version inside =
  let () =
    match !version with
    | None | Some Semver.{ major = 0; minor = 14; _ } -> ()
    | Some version ->
      Fmt.failwith "schematic ppx version mismatch: expecting 0.14.x, got %a"
        Semver.pp version Ast_pattern.parse
  in
  let sig_type_decl =
    let arg =
      let open Deriving.Args in
      empty
    in
    Deriving.Generator.V2.make arg expand_sig
  and str_type_decl =
    let arg =
      let open Deriving.Args in
      let fixpoint = flag "fixpoint"
      and gadt = arg "gadt" (elist (pexp_ident (lident __)))
      and jsoo = arg "jsoo" __
      and sqml = arg "sqml" (estring __) in
      empty +> fixpoint +> gadt +> jsoo +> sqml
    in
    Deriving.Generator.V2.make arg (expand ~version ~inside)
  in
  Deriving.add ~sig_type_decl ~str_type_decl "schema"

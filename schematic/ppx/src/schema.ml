open Base
open Ppxlib
open Utils
open Intf

type closed = value_binding * value_binding
type configuration = unit

(** An AST expression evaluating to a schema. *)
type expanded =
  | Components of {
      descriptor : expression;
      id : expression;
      parametric : expression;
    }
      (** Chunks of AST that evaluate to the corresponding fields of a
          [Schema.t]. Equilavent to the [Expression] constructor if combined in
          such a way (see the [schema_of_expanded] helper), but enables the
          caller to manipulate the individual fields if needed. *)
  | Expression of expression
      (** Arbitrary expression evaluating to a schema. *)

let schema_name s = name "schema" s
let schema_name_versioned s = name "schema_versioned" s

let schema_anonymous_name_versioned i =
  schema_name_versioned @@ Fmt.str "%i_any" i

let schema_id_versioned ?prefix ~loc = suffix_id ~loc ?prefix "schema_versioned"

let expanded_of_descriptor ~loc ?(id = [%expr None])
    ?(parametric = [%expr None]) descriptor =
  Components { descriptor; id; parametric }

let schema_of_expanded ~loc = function
  | Components { descriptor; id; parametric } ->
    [%expr
      {
        Schema.descriptor = [%e descriptor];
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
            [%e schema_of_expanded ~loc @@ encode_type ~context ~type_context t]]
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
            [%e schema_of_expanded ~loc @@ encode_type ~context ~type_context t]]
    (* Parametric type *)
    | { ptyp_desc = Ptyp_constr ({ txt; loc }, args); _ } ->
      let schema_f = Ast.pexp_ident { loc; txt = schema_id_versioned ~loc txt }
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
        let mapper ~construct ~deconstruct =
          [%expr
            Schema.Tuple
              {
                decode = [%e construct];
                encode = [%e deconstruct];
                items = [%e tuple_items ~context ~type_context types];
              }]
        in

        combinator ~loc types field_name field_value constructed deconstructed
          mapper
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

and tuple_items ~context ~type_context types =
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
  [%expr [%e items]]

and combinator :
    type f.
    loc:location ->
    f list ->
    (int -> f -> location * label) ->
    (int -> f -> expression) ->
    expression ->
    pattern ->
    (construct:expression -> deconstruct:expression -> expression) ->
    expression =
 fun ~loc fields field_name field_value constructed deconstructed mapper ->
  let module Ast = (val Ast_builder.make loc) in
  let construct =
    let pattern =
      let pattern i f =
        let loc, txt = field_name i f in
        Ast.ppat_var { loc; txt }
      in
      let f i tl field =
        let pattern = pattern i field in
        [%pat? [%p pattern] :: [%p tl]]
      in
      List.fold_righti ~f ~init:[%pat? []] fields
    in
    Ast.pexp_fun Nolabel None [%pat? Hlist.([%p pattern])] constructed
  and deconstruct =
    let value =
      let expr i f = field_value i f in
      let f i tl field =
        let expr = expr i field in
        [%expr [%e expr] :: [%e tl]]
      in
      List.fold_righti ~f ~init:[%expr []] fields
    in
    Ast.pexp_fun Nolabel None deconstructed [%expr Hlist.([%e value])]
  in
  mapper ~construct ~deconstruct

and expand_record ~context ~type_context fields =
  _expand_record false ~context ~type_context fields

and _expand_record as_tuple ~context ~type_context
    (fields : Attributes.field list) =
  let module Ast = (val Ast_builder.make type_context.loc) in
  let loc = type_context.loc in
  let type_ =
    (* This type is used to explicitely disambiguate mapped values in case of
       record field name conflicts, otherwise a schema can end up manipulating a
       completely different type. *)
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
      let alias = version_match ~f:Ast.estring (Ast.estring (rename name)) alias
      and schema =
        let schema =
          version_match
            ~f:(fun schema -> [%expr [%e schema] __schematic_version])
            (schema_of_expanded ~loc @@ encode_type ~context ~type_context type_)
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
      let deconstructed = Ast.ppat_record (List.filter_map ~f fields) Closed in
      [%pat? ([%p deconstructed] : [%t type_])]
  in
  let mapped ~construct ~deconstruct =
    [%expr
      Schema.Object
        {
          decode = [%e construct];
          encode = [%e deconstruct];
          fields = [%e fields_schema];
        }]
  in
  expanded_of_descriptor ~loc
  @@ combinator ~loc fields field_name field_value constructed deconstructed
       mapped

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
          ~computed:(match pcd_args with Pcstr_record _ -> true | _ -> false)
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
  let f (txt, pcd_args, loc, Attributes.{ alias; aliases; computed; _ }) =
    let module Ast = (val Ast_builder.make loc) in
    let name =
      version_match ~f:Ast.estring (Ast.estring (String.lowercase txt)) alias
    and name_aliases =
      match aliases with
      | [] -> None
      | _ ->
        Some
          (version_match
             ~f:(Fn.compose Ast.elist (List.map ~f:Ast.estring))
             (Ast.elist []) aliases)
    in
    let schema, inline, ctor_pattern, ctor_value, pattern, value =
      match constructor_arguments_normalize ~loc pcd_args with
      | None ->
        let schema =
          if Option.is_none type_context.attributes.union_key then
            match name_aliases with
            | None -> expanded_of_descriptor ~loc [%expr String_const [%e name]]
            | Some aliases ->
              (* FIXME: aliases, polymorphic variants and C codegen don't
                 interact well.

                 Encountering an alias wil most likely break C codegen for
                 now. *)
              expanded_of_descriptor ~loc
                [%expr
                  let __schematic_case name =
                    Schema.Case
                      {
                        name;
                        (* FIXME: see above. *)
                        raw_name = name;
                        schema =
                          {
                            descriptor = String_const name;
                            id = None;
                            parametric = None;
                          };
                        inline = false;
                        encoder = (fun () -> Some ());
                        decoder = (fun () -> ());
                      }
                  in
                  Union
                    {
                      key = None;
                      polymorphic = false;
                      cases =
                        __schematic_case [%e name]
                        :: Stdlib.List.map __schematic_case [%e aliases];
                    }]
          else
            expanded_of_descriptor ~loc
              [%expr
                Object
                  {
                    fields = FieldEnd;
                    encode = (fun () -> Hlist.[]);
                    decode = (fun Hlist.[] -> ());
                  }]
        in
        (schema, false, None, None, None, None)
      | Some { ctor_pattern; ctor_value; inline; pattern; type_; value; _ } ->
        let p i = Ast.pvar (Fmt.str "f%i" i)
        and v i _ = Ast.evar (Fmt.str "f%i" i) in
        let schema =
          match (aliases, pcd_args) with
          | { loc; _ } :: _, _ ->
            expanded_of_descriptor ~loc:type_context.loc
              [%expr
                [%ocaml.error
                  "schematic.aliases attribute not supported on record \
                   payloads yet"]]
          | [], Pcstr_record fields ->
            let computed_fields = Attributes.fields_of_computed ~loc computed in
            _expand_record true ~context ~type_context
              (List.map ~f:(expand_field context) fields @ computed_fields)
          | [], _ -> encode_type ~context ~type_context type_
        in
        ( schema,
          inline,
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
            decoder = [%e destruct];
            encoder = [%e construct];
            inline = [%e Ast.ebool inline];
            name = [%e name];
            raw_name = [%e Ast.estring txt];
            schema = [%e schema_of_expanded ~loc schema];
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
          polymorphic = [%e Ast.ebool poly];
          cases = [%e Ast.elist (List.filter_map ~f options)];
          key = [%e key];
        }]

let expand_abstract_type ~context ~type_context t =
  encode_type ~context ~type_context t

let type_wrap context type_context t =
  let loc = type_context.loc in
  let module Ast = (val Ast_builder.make loc) in
  let wrap param t =
    match param with
    | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ }, _ ->
      if context.global.inside then
        [%type: [%t Ast.ptyp_var name] Schema.t -> [%t t]]
      else [%type: [%t Ast.ptyp_var name] Schematic.Schema.t -> [%t t]]
    | { ptyp_desc = Ptyp_any; ptyp_loc = loc; _ }, _ -> [%type: _ -> [%t t]]
    | t, _ ->
      Ppxlib.Location.raise_errorf ~loc
        "schematic-ppx: unsupported type parameter A %a" Pprintast.core_type t
  in
  List.fold_right ~f:wrap ~init:t type_context.params

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
      [%expr Some [%e Ast.estring str]]
    in
    Option.value ~default type_context.attributes.name
  in
  let params_wrap body =
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
          "schematic-ppx: unsupported type parameter B %a" Pprintast.core_type t
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
          "schematic-ppx: unsupported type parameter C %a" Pprintast.core_type t
    in
    List.foldi ~f:wrap ~init:f type_context.params
  in
  let schema, cache =
    let id =
      let f param exp =
        match param with
        | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ }, _ ->
          let combine =
            if context.global.inside then [%expr Schema.Name.combine]
            else [%expr Schematic.Schema.Name.combine]
          in
          [%expr
            match [%e exp] with
            | None -> None
            | Some id ->
              [%e combine]
                [%e Ast.evar (param_prefix ^ schema_name_versioned name)]
                id]
        | [%type: _], _ -> exp
        | t, _ ->
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: unsupported type parameter D %a" Pprintast.core_type
            t
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
              Plain
                { name = __schematic_schema_id; descriptor = [%e descriptor] }]
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
                params_wrap
                  [%expr
                    match
                      Base.Map.find !__schematic_cache
                        __schematic_schema_version
                    with
                    | Some schema -> schema
                    | None ->
                      Schema.fixpoint (fun __schematic_self ->
                          match
                            Base.Map.find !__schematic_cache
                              __schematic_schema_version
                          with
                          | Some schema -> schema
                          | None ->
                            let () =
                              __schematic_cache :=
                                Base.Map.set !__schematic_cache
                                  ~key:__schematic_schema_version
                                  ~data:__schematic_self
                            in
                            let res = [%e c.descriptor] in
                            let () =
                              __schematic_cache :=
                                Base.Map.set !__schematic_cache
                                  ~key:__schematic_schema_version ~data:res
                            in
                            res)]
              else params_wrap c.descriptor]
          in
          let __schematic_parametric : _ Schema.parametric option =
            match [%e schema_id] with
            | None -> None
            | Some __schematic_schema_id -> [%e parametric]
          and id = [%e id] in
          {
            Schema.descriptor = [%e descriptor];
            id =
              (match __schematic_parametric with
              | None -> id
              | Some parametric -> (
                match Schema.Name.of_parametric parametric with
                | None -> None
                | Some name -> Some (Schema.Name.to_id name)));
            parametric = __schematic_parametric;
          }],
        cache )
    | Expression e ->
      let e =
        if context.global.inside then
          [%expr { ([%e e]) with Schema.id = [%e id] }]
        else [%expr { ([%e e]) with Schematic.Schema.id = [%e id] }]
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
      if context.global.inside then
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
      if context.global.inside then
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
        (type_wrap context type_context
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

let expand_sig ~(context : context) types =
  let loc = context.loc in
  let module Ast = (val Ast_builder.make loc) in
  let f ({ ptype_name = { txt; loc }; ptype_params; _ }, type_context) =
    let type_ =
      Ast.ptyp_constr
        { txt = Longident.Lident txt; loc }
        (List.map ~f:fst ptype_params)
    in
    [
      Ast.(
        psig_value
        @@ value_description
             ~name:{ txt = name "schema_versioned" txt; loc }
             ~type_:
               [%type:
                 Schematic.Schema.version ->
                 [%t
                   type_wrap context type_context
                     [%type: [%t type_] Schematic.Schema.t]]]
             ~prim:[]);
      Ast.(
        psig_value
        @@ value_description
             ~name:{ txt = name "schema" txt; loc }
             ~type_:
               (type_wrap context type_context
                  [%type: [%t type_] Schematic.Schema.t])
             ~prim:[]);
    ]
  in
  List.map ~f types |> List.concat

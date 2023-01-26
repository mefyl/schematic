open Base
open Ppxlib

module Ast_pattern = struct
  include Ast_pattern

  let ( let+ ) v f = map1 ~f v

  let drop () =
    let f _ _ _ cont = cont in
    of_func f
end

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

let param_prefix = "__schematic_param_"
let expect ~loc pattern ast = Ast_pattern.parse pattern loc ast Fn.id

let rename name =
  let len = String.length name in
  if Char.(name.[len - 1] = '_') then String.sub name ~pos:0 ~len:(len - 1)
  else name

let name suffix s =
  let s = rename s in
  if String.(s = "t") then suffix else s ^ "_" ^ suffix

let suffix_id ~loc ?(prefix = "") suffix = function
  | Lident n -> Lident (prefix ^ name suffix n)
  | Ldot (q, n) -> Ldot (q, prefix ^ name suffix n)
  | Lapply (_, _) ->
    Ppxlib.Location.raise_errorf ~loc "Functor application found as a type"

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

let version_match ~f init options =
  let f next = function
    | { txt = schema, None; _ } -> f schema
    | {
        txt =
          schema, Some (Semver.{ major; minor; patch; prerelease; build }, op);
        loc;
      } ->
      let module Ast_builder = Ast_builder.Make (struct
        let loc = loc
      end) in
      let version =
        [%expr
          Schematic.Version.make
            ~prerelease:
              [%e
                Ast_builder.elist @@ List.map ~f:Ast_builder.estring prerelease]
            ~build:
              [%e Ast_builder.elist @@ List.map ~f:Ast_builder.estring build]
            [%e Ast_builder.eint major] [%e Ast_builder.eint minor]
            [%e Ast_builder.eint patch]]
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

let expr_option ~loc expr = function
  | None -> [%expr None]
  | Some e -> [%expr Some [%e expr ~loc e]]

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

let expand_field _
    {
      pld_attributes;
      pld_loc = loc;
      pld_name = { txt = name; _ };
      pld_type;
      pld_mutable;
      _;
    } =
  let read_attribute (attrs : Attributes_intf.field)
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
                  (Attributes_intf.Source.merge ~loc "default"
                     attrs.specs.default (f default));
              omit;
            };
        }
      | _ ->
        Ppxlib.Location.raise_errorf ~loc
          "schematic-ppx: expression payload expected"
    in
    match attr_name.txt with
    | "default" -> default Attributes_intf.Source.generic true false
    | "schematic.as" ->
      let pattern = Ast_pattern.(versioned (estring __) (estring __)) in
      {
        attrs with
        alias = { txt = expect ~loc pattern attr_payload; loc } :: attrs.alias;
      }
    | "schematic.default" -> default Attributes_intf.Source.schematic true false
    | "schematic.default_opt" ->
      default Attributes_intf.Source.schematic false false
    | ("schematic.inline" as attr) | ("schematic.inline_try" as attr) -> (
      match attr_payload with
      | PStr [] ->
        if Option.is_none attrs.inline then
          let v =
            if String.(attr = "schematic.inline") then Attributes_intf.Inline
            else Inline_try
          in
          { attrs with inline = Some v }
        else
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: inline specified multiple times"
      | _ ->
        Ppxlib.Location.raise_errorf ~loc "schematic-ppx: no payload expected")
    | "schematic.omit" -> default Attributes_intf.Source.schematic true true
    | "schematic.omit_opt" ->
      default Attributes_intf.Source.schematic false true
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
              Attributes_intf.
                {
                  specs with
                  description =
                    Some (expect Ast_pattern.(estring __) ~loc value);
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
      {
        attrs with
        schema = { txt = expect ~loc pattern attr_payload; loc } :: attrs.schema;
      }
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
      let () = Attributes_intf.check_unknown_attribute ~loc name in
      attrs
  in
  List.fold_left ~f:read_attribute
    ~init:
      (Attributes_intf.make_field ~loc
         ~mutable_:Poly.(pld_mutable = Mutable)
         ~type_:pld_type name)
    pld_attributes

module Warning = struct
  let make ~loc n =
    let module Ast = (val Ast_builder.make loc) in
    Ast.attribute ~name:{ txt = "warning"; loc }
      ~payload:
        (PStr [ { pstr_desc = Pstr_eval (Ast.estring n, []); pstr_loc = loc } ])

  let unused_var_strict ~loc = make ~loc "-unused-var-strict"
  let unused_rec ~loc = make ~loc "-unused-rec-flag"
end

let unused ~loc t =
  let open (val Ast_builder.make loc) in
  {
    t with
    ptype_attributes =
      {
        attr_name = Located.mk "warning";
        attr_payload =
          PStr [ pstr_eval (estring "-unused-type-declaration") [] ];
        attr_loc = loc;
      }
      :: t.ptype_attributes;
  }

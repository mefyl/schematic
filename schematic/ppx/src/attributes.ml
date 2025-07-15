open Base
open Utils
open Ppxlib
include Attributes_intf

let requirement { computed; loc; specs = { default; _ }; type_; _ } =
  match type_ with
  | [%type: [%t? t] option] when Option.is_none default ->
    if Option.is_none computed then (Optional, t) else (Computed_optional, t)
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

(** Schematic specific attributes that may be attached to types. *)
type t = {
  alias : string versioned; [@default []]  (** [@schematic.alias] attribute. *)
  aliases : string list versioned; [@default []]
      (** [[@schematic.aliases]] attribute. *)
  computed : (string, expression * core_type, String.comparator_witness) Map.t;
      [@default Map.empty (module String)]
      (** [@schematic.computed] attribute *)
  gadt_variant : string option;  (** [[@schematic.gadt]] attribute. *)
  name : expression option;  (** [[@schematic.name]] attribute. *)
  schema : expression option;  (** [@schematic.schema] attribute *)
  since : Semver.t option;
  union_key : string option;  (** [@schematic.union_key] attribute*)
}
[@@deriving make]

let empty = make ()

let field_of_computed ~loc (name, (value, type_)) =
  make_field ~loc:value.pexp_loc ~type_ ~computed:[%expr Some [%e value]] name

let fields_of_computed ~loc computed =
  Map.to_sequence computed
  |> Sequence.map ~f:(field_of_computed ~loc)
  |> Sequence.to_list

let rec extract ?(alias = false) ?(computed = false) ?(gadt_variant = false)
    ?(name = false) ?(schema = false) ?(since = false) ?(union_key = false) =
  let merge name ~loc old v =
    match old with
    | None -> Some v
    | Some _ ->
      Ppxlib.Location.raise_errorf ~loc
        "schematic-ppx: duplicate schematic.%s annotation on schema" name
  in
  let attribute_name ~loc opt attr_payload attrs =
    match attrs with
    | { name = None; _ } ->
      let expr =
        expect ~loc Ast_pattern.(single_expr_payload __) attr_payload
      in
      { attrs with name = Some (if opt then expr else [%expr Some [%e expr]]) }
    | _ ->
      Ppxlib.Location.raise_errorf ~loc
        "schematic-ppx: duplicate name annotation on schema"
  in
  function
  | { attr_name = { txt; _ }; attr_loc = loc; attr_payload } :: attrs -> (
    let attrs = extract ~alias ~computed ~name ~union_key attrs in
    match txt with
    | "schematic.as" when alias ->
      let alias =
        expect
          Ast_pattern.(versioned (estring __) (estring __))
          ~loc attr_payload
      in
      { attrs with alias = attrs.alias @ [ { txt = alias; loc } ] }
    | "schematic.aliases" when alias ->
      let aliases =
        expect
          Ast_pattern.(versioned (elist (estring __)) (elist (estring __)))
          ~loc attr_payload
      in
      { attrs with aliases = attrs.aliases @ [ { txt = aliases; loc } ] }
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
    | "schematic.name" when name -> attribute_name ~loc false attr_payload attrs
    | "schematic.name_opt" when name ->
      attribute_name ~loc true attr_payload attrs
    | "schematic.since" when since ->
      let pattern =
        let semver_of_string repr =
          match Semver.of_string repr with
          | Some version -> version
          | None ->
            Ppxlib.Location.raise_errorf ~loc
              "schematic-ppx: invalid semantic version: %S" repr
        in
        let open Ast_pattern in
        map_result ~f:semver_of_string @@ single_expr_payload (estring __)
      in
      {
        attrs with
        since =
          merge "since" ~loc attrs.since @@ expect ~loc pattern attr_payload;
      }
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
      alias = [];
      aliases = [];
      computed = Map.empty (module String);
      gadt_variant = None;
      name = None;
      schema = None;
      since = None;
      union_key = None;
    }

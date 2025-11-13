open Base
open Ppxlib
open Utils

module type S = sig
  (** Key in the [~plugin] deriver argument. *)
  val key : string

  (** Pretty name of the plugin *)
  val name : string

  (** Type of the configuration in the [~plugin] deriver key. *)
  type configuration

  (** Shape of the configuration in the [~plugin] deriver key. *)
  val configuration_pattern :
    unit -> (expression, configuration -> 'a, 'a) Ast_pattern.t

  (** Short description for the plugin. *)
  val synopsis : string

  val read_attribute :
    loc:Ppxlib.Location.t ->
    string ->
    payload ->
    Attributes_intf.field ->
    Attributes_intf.field

  module Expansion : Expansion.S with type configuration = configuration
end

type t = (module S)

let plugins = ref ([] : t list)
let register plugin = plugins := plugin :: !plugins

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
    | "schematic.since" ->
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
      (match attrs.since with
      | None ->
        { attrs with since = Some (expect ~loc pattern attr_payload) }
      | Some _ ->
        Ppxlib.Location.raise_errorf ~loc
          "schematic-ppx: duplicate schematic.since annotation on field")
    | name -> (
      match String.split ~on:'.' name with
      | "schematic" :: plugin :: attribute -> (
        match
          List.find !plugins ~f:(fun (module Plugin) ->
              String.(Plugin.key = plugin))
        with
        | None ->
          Ppxlib.Location.raise_errorf ~loc "schematic-ppx: unknown plugin %s"
            plugin
        | Some (module Plugin) ->
          Plugin.read_attribute ~loc
            (String.concat ~sep:"." attribute)
            attr_payload attrs)
      | _ ->
        let () = Attributes_intf.check_unknown_attribute ~loc name in
        attrs)
  in
  List.fold_left ~f:read_attribute
    ~init:
      (Attributes_intf.make_field ~loc
         ~mutable_:Poly.(pld_mutable = Mutable)
         ~type_:pld_type name)
    pld_attributes

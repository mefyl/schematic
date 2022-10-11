open Base
open Utils
open Ppxlib
open Intf

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

module Jsoo_type_expander = Expander (Jsoo)
module Schema_expander = Expander (Schema)
module Sqml_binding_expander = Expander (Sqml)

let expand ~ctxt ~global (recursive, types) fixpoint gadt jsoo sqml =
  let loc = Ppxlib.Expansion_context.Deriver.derived_item_loc ctxt in
  let jsoo =
    if global.enable_jsoo then
      match jsoo with
      | None -> None
      | Some [%expr jsoo] | Some [%expr true] -> Some true
      | Some [%expr false] -> Some false
      | Some exp ->
        Ppxlib.Location.raise_errorf ~loc:exp.pexp_loc
          "schematic-ppx: invalid ~jsoo argument value: %a" Pprintast.expression
          exp
    else None
  in
  let context =
    let recursive = Poly.(recursive = Recursive)
    and code_path = Ppxlib.Expansion_context.Deriver.code_path ctxt in
    let types =
      let f { ptype_name; _ } = ptype_name.txt in
      if recursive then List.map ~f types else []
    in
    { code_path; fixpoint; gadt_variant = None; global; loc; types; recursive }
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
            ~configuration:Jsoo.{ functions = false }
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
    else if global.inside then
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

let expand_sig ~ctxt ~global:_ (_recursive, types) =
  (* FIXME: Jsoo declarations *)
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

let add ~inside ~jsoo ~version =
  let global =
    lazy
      (let () =
         match !version with
         | None | Some Semver.{ major = 0; minor = 15; _ } -> ()
         | Some version ->
           Fmt.failwith
             "schematic ppx version mismatch: expecting 0.15.x, got %a"
             Semver.pp version Ast_pattern.parse
       in
       { inside; enable_jsoo = !jsoo; version = !version })
  in
  let sig_type_decl =
    let arg =
      let open Deriving.Args in
      empty
    in
    Deriving.Generator.V2.make arg (fun ~ctxt ast ->
        expand_sig ~global:(Lazy.force global) ~ctxt ast)
  and str_type_decl =
    let arg =
      let open Deriving.Args in
      let fixpoint = flag "fixpoint"
      and gadt = arg "gadt" (elist (pexp_ident (lident __)))
      and jsoo = arg "jsoo" __
      and sqml = arg "sqml" (estring __) in
      empty +> fixpoint +> gadt +> jsoo +> sqml
    in
    Deriving.Generator.V2.make arg (fun ~ctxt ast ->
        expand ~global:(Lazy.force global) ~ctxt ast)
  in
  Deriving.add ~sig_type_decl ~str_type_decl "schema"

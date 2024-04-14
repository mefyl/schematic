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

let type_context (t : type_declaration) =
  let computed = match t.ptype_kind with Ptype_record _ -> true | _ -> false
  and union_key =
    match (t.ptype_kind, t.ptype_manifest) with
    | Ptype_variant _, _
    | Ptype_abstract, Some { ptyp_desc = Ptyp_variant _; _ } ->
      true
    | _ -> false
  in
  {
    attributes =
      Attributes.extract ~computed ~name:true ~union_key t.ptype_attributes;
    loc = t.ptype_loc;
    name = t.ptype_name.txt;
    params = t.ptype_params;
  }

let jsoo_flag ~context jsoo =
  if context.global.enable_jsoo then
    match jsoo with
    | None -> None
    | Some [%expr jsoo] | Some [%expr true] -> Some true
    | Some [%expr false] -> Some false
    | Some exp ->
      Ppxlib.Location.raise_errorf ~loc:exp.pexp_loc
        "schematic-ppx: invalid ~jsoo argument value: %a" Pprintast.expression
        exp
  else None

let type_ ~context ~type_context t =
  let loc = type_context.loc in
  match t.ptype_kind with
  | Ptype_record fields ->
    let fields = List.map ~f:(expand_field context) fields
    and computed =
      Attributes.fields_of_computed ~loc type_context.attributes.computed
    in
    Record (fields @ computed)
  | Ptype_abstract -> (
    match t.ptype_manifest with Some type_ -> Core type_ | None -> Abstract)
  | Ptype_open ->
    Ppxlib.Location.raise_errorf ~loc "schematic-ppx: unsupported Ptype_open"
  | Ptype_variant options -> Variant options

let expand ~context types gadt jsoo sqml =
  let jsoo = jsoo_flag ~context jsoo
  and loc = context.loc in
  let expand context =
    let types =
      let f t =
        let type_context = type_context t in
        (type_ ~context ~type_context t, type_context)
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
      [%str open Schematic_jsoo.Schemas [@@warning "-unused-open"]] @ contents
    else if context.global.inside then
      [%str open Schemas [@@warning "-unused-open"]] @ contents
    else [%str open Schematic.Schemas [@@warning "-unused-open"]] @ contents
  in
  match gadt with
  | Some variants ->
    let f variant = expand { context with gadt_variant = Some variant } in
    List.map ~f variants |> List.concat
  | None -> expand context

let expand_sig ~(context : context) types jsoo =
  let loc = context.loc
  and jsoo = jsoo_flag ~context jsoo
  and types = List.map types ~f:(fun t -> (t, type_context t)) in
  let contents =
    match jsoo with
    | None -> []
    | Some functions ->
      let f (t, type_context) =
        Jsoo.expand_sig ~context ~configuration:{ functions } ~type_context
          (type_ ~context ~type_context t)
      in
      List.map ~f types |> List.concat
  in
  let contents = Schema.expand_sig ~context types @ contents in
  if Option.is_some jsoo then
    [%sig: open Schematic_jsoo.Schemas [@@warning "-unused-open"]] @ contents
  else if context.global.inside then
    [%sig: open Schemas [@@warning "-unused-open"]] @ contents
  else [%sig: open Schematic.Schemas [@@warning "-unused-open"]] @ contents

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
  let context ~ctxt ~fixpoint recursive types =
    let code_path = Ppxlib.Expansion_context.Deriver.code_path ctxt
    and loc = Ppxlib.Expansion_context.Deriver.derived_item_loc ctxt
    and recursive = Poly.(recursive = Recursive) in
    let types =
      let f { ptype_name; _ } = ptype_name.txt in
      if recursive then List.map ~f types else []
    in
    {
      code_path;
      fixpoint;
      gadt_variant = None;
      global = Lazy.force global;
      loc;
      types;
      recursive;
    }
  in
  let () =
    let sig_type_decl =
      let arg =
        let open Deriving.Args in
        let fixpoint = flag "fixpoint"
        and jsoo = arg "jsoo" __ in
        empty +> fixpoint +> jsoo
      in
      Deriving.Generator.V2.make arg (fun ~ctxt (recursive, types) fixpoint ->
          expand_sig ~context:(context ~ctxt ~fixpoint recursive types) types)
    and str_type_decl =
      let arg =
        let open Deriving.Args in
        let fixpoint = flag "fixpoint"
        and gadt = arg "gadt" (elist (pexp_ident (lident __)))
        and jsoo = arg "jsoo" __
        and sqml = arg "sqml" (estring __) in
        empty +> fixpoint +> gadt +> jsoo +> sqml
      in
      Deriving.Generator.V2.make arg (fun ~ctxt (recursive, types) fixpoint ->
          expand ~context:(context ~ctxt ~fixpoint recursive types) types)
    in
    let _deriver = Deriving.add ~sig_type_decl ~str_type_decl "schema" in
    ()
  and () =
    let extension =
      let extracter = Ast_pattern.(ptyp __)
      and expand ~ctxt type' =
        let loc = Ppxlib.Expansion_context.Extension.extension_point_loc ctxt in
        let schema =
          let context =
            let code_path = Ppxlib.Expansion_context.Extension.code_path ctxt in
            {
              code_path;
              fixpoint = false;
              gadt_variant = None;
              global = Lazy.force global;
              loc;
              types = [];
              recursive = false;
            }
          in
          Schema.expand_abstract_type ~context
            ~type_context:
              { attributes = Attributes.empty; loc; name = ""; params = [] }
            type'
          |> Schema.schema_of_expanded ~loc
        in
        [%expr
          let __schematic_version _ = None in
          [%e schema]]
      in
      Extension.V3.declare "schema" Extension.Context.expression extracter
        expand
    in
    Driver.register_transformation
      ~rules:[ Context_free.Rule.extension extension ]
      "schematic"
  in
  ()

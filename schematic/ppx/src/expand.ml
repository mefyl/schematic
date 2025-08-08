open Base
open Utils
open Ppxlib
open Intf
module Jsoo_type_expander = Expansion.M (Jsoo)
module Schema_expander = Expansion.M (Schema)

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
    let fields = List.map ~f:(Plugin.expand_field context) fields
    and computed =
      Attributes.fields_of_computed ~loc type_context.attributes.computed
    in
    Record (fields @ computed)
  | Ptype_abstract -> (
    match t.ptype_manifest with Some type_ -> Core type_ | None -> Abstract)
  | Ptype_open ->
    Ppxlib.Location.raise_errorf ~loc "schematic-ppx: unsupported Ptype_open"
  | Ptype_variant options -> Variant options

let expand ~context types gadt jsoo plugins =
  let jsoo = jsoo_flag ~context jsoo
  and loc = context.loc
  and plugins = Option.value ~default:[] plugins in
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
        let detect_plugin contents (module Plugin : Plugin.S) =
          match List.Assoc.find ~equal:String.equal plugins Plugin.key with
          | None -> contents
          | Some payload ->
            Ast_pattern.parse (Plugin.configuration_pattern ()) loc payload
            @@ fun configuration ->
            let module Expander = Expansion.M (Plugin.Expansion) in
            Expander.expand ~configuration ~context types @ contents
        in
        List.fold !Plugin.plugins ~init:contents ~f:detect_plugin
      in
      Schema_expander.expand ~configuration:() ~context types @ contents
    in
    if Option.is_some jsoo then
      [%str open Schematic_jsoo.Schemas [@@warning "-unused-open"]] @ contents
    else if context.global.inside then
      [%str open Schemas [@@warning "-unused-open"]] @ contents
    else
      (* We are not including Schematic.Schemas anymore to avoid hiding user code.
       * This causes us to:
       *
       * - Have to write all this boilerplate.
       * - Prevents us from automatically exposing [Ptime],[Uri], ... because that
       *   would fail if the client code don't link with them.
       *)
      [%str
        open struct
          module Schema = Schematic.Schema

          let array_schema = Schematic.Schemas.array_schema
          let array_schema_versioned = Schematic.Schemas.array_schema_versioned
          let pp_ignore = Schematic.Schemas.pp_ignore
          let equal_ignore = Schematic.Schemas.equal_ignore
          let compare_ignore = Schematic.Schemas.compare_ignore
          let bool_schema = Schematic.Schemas.bool_schema
          let bool_schema_versioned = Schematic.Schemas.bool_schema_versioned
          let char_schema = Schematic.Schemas.char_schema
          let char_schema_versioned = Schematic.Schemas.char_schema_versioned
          let float_schema = Schematic.Schemas.float_schema
          let float_schema_versioned = Schematic.Schemas.float_schema_versioned
          let ignore_schema = Schematic.Schemas.ignore_schema

          let ignore_schema_versioned =
            Schematic.Schemas.ignore_schema_versioned

          let int64_schema = Schematic.Schemas.int64_schema
          let int64_schema_versioned = Schematic.Schemas.int64_schema_versioned
          let int_schema = Schematic.Schemas.int_schema
          let int_schema_versioned = Schematic.Schemas.int_schema_versioned
          let json_schema = Schematic.Schemas.json_schema
          let json_schema_versioned = Schematic.Schemas.json_schema_versioned
          let equal_json = Schematic.Schemas.equal_json
          let list_schema = Schematic.Schemas.list_schema
          let list_schema_versioned = Schematic.Schemas.list_schema_versioned
          let mapping_schema = Schematic.Schemas.mapping_schema
          let option_schema = Schematic.Schemas.option_schema

          let option_schema_versioned =
            Schematic.Schemas.option_schema_versioned

          let string_const_schema = Schematic.Schemas.string_const_schema
          let string_schema = Schematic.Schemas.string_schema

          let string_schema_versioned =
            Schematic.Schemas.string_schema_versioned

          let unit_schema = Schematic.Schemas.unit_schema
          let unit_schema_versioned = Schematic.Schemas.unit_schema_versioned

          module Bool = struct
            include Schematic.Schemas.Bool
            include Bool
          end

          module Char = struct
            include Schematic.Schemas.Char
            include Char
          end

          (* module Either = struct *)
          (*   include Schematic.Schemas.Either *)
          (*   include Either *)
          (* end *)

          module Int = struct
            include Schematic.Schemas.Int
            include Int
          end

          (* module Ptime = struct *)
          (*   include Schematic.Schemas.Ptime *)
          (*   include Ptime *)
          (* end *)

          module Option = struct
            include Schematic.Schemas.Option
            include Option
          end

          module Result = struct
            include Schematic.Schemas.Result
            include Result
          end

          module String = struct
            include Schematic.Schemas.String
            include String
          end

          (* module Uri = struct *)
          (*   include Schematic.Schemas.Uri *)
          (*   include Uri *)
          (* end *)
        end [@@warning "-unused-open"]]
      @ contents
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
             Semver.pp version
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
        and plugins =
          arg "plugins"
          @@ pexp_record (many @@ pack2 @@ pair (loc (lident __)) __) none
        in
        empty +> fixpoint +> gadt +> jsoo +> plugins
      in
      Deriving.Generator.V2.make arg (fun ~ctxt (recursive, types) fixpoint ->
          expand ~context:(context ~ctxt ~fixpoint recursive types) types)
    in
    let _deriver = Deriving.add ~sig_type_decl ~str_type_decl "schema" in
    ()
  and () =
    let extension_schema =
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
          |> Schema.schema_of_expanded ~context ~loc
        in
        [%expr
          begin [@warning "-unused-open"]
            let open Schematic.Schemas in
            let __schematic_version _ = None in
            [%e schema]
          end]
      in
      Extension.V3.declare "schema" Extension.Context.expression extracter
        expand
    and extension_version =
      let extracter = Ast_pattern.(single_expr_payload @@ estring __)
      and expand ~ctxt repr =
        let loc = Ppxlib.Expansion_context.Extension.extension_point_loc ctxt in
        let open (val Ast_builder.make loc) in
        match Semver.of_string repr with
        | Some _ ->
          [%expr
            Schematic.Version.of_string_exn
              [%e Ast_builder.Default.estring ~loc repr]]
        | None ->
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: invalid semantic version: %S" repr
      in
      Extension.V3.declare "version" Extension.Context.expression extracter
        expand
    in
    Driver.register_transformation
      ~rules:
        [
          Context_free.Rule.extension extension_schema;
          Context_free.Rule.extension extension_version;
        ]
      "schematic"
  in
  ()

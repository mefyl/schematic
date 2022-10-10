open Base
open Ppxlib
open Utils
open Intf

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
  let f
      Attributes.(
        { alias; inline; loc; mutable_; name; type_; _ } as attributes) =
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
          let js_t =
            match context.global.version with
            | Some { major = 0; minor = 13; patch; _ } when patch < 1 ->
              expand_type ~loc ~context ~type_context type_
            | _ -> (
              match Attributes.requirement attributes with
              | (Computed | Required), type_ ->
                expand_type ~loc ~context ~type_context type_
              | (Computed_optional | Default_opt _ | Optional), type_ ->
                [%type:
                  [%t expand_type ~loc ~context ~type_context type_]
                  Js_of_ocaml.Js.Optdef.t])
          in
          if mutable_ then [%type: [%t js_t] Js_of_ocaml.Js.prop]
          else [%type: [%t js_t] Js_of_ocaml.Js.readonly_prop]
        and alias =
          List.find_map
            ~f:(function { txt = alias, None; _ } -> Some alias | _ -> None)
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
    match (context.global.version, type_context.attributes.union_key) with
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
            | { ptyp_desc = Ptyp_var n; _ } -> Some (loc, Fmt.str "%s_schema" n)
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
                             (t0 -> res Js_of_ocaml.Js.t) Js_of_ocaml.Js.constr)
                           (t0 : t0) (_ : unit) : res Js_of_ocaml.Js.t ->
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
  if functions then type_ :: List.filter_map ~f:converters items else [ type_ ]

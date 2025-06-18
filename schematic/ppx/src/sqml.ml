open Base
open Ppxlib
open Schematic_deriver
open Utils
open Intf

module Sqml = struct
  let key = "sqml"
  let name = "SQML"

  type configuration = string

  let configuration_pattern () = Ast_pattern.(estring __)
  let synopsis = "SQML table metadata deriver"

  module Expansion = struct
    type closed = structure_item list
    type configuration = string

    type expanded = {
      columns : structure_item list;
      column_explosions : structure_item list;
      columns_list : expression;
      columns_type : core_type;
      fields : Attributes.field list;
      expr_explosions : structure_item list;
      primary_type : core_type;
      primary : expression;
      record : expression;
      record_pattern : pattern;
    }

    let explode_columns ~loc (fields : Attributes.field list) init =
      List.fold_right fields ~init ~f:(fun field e ->
          [%expr
            let ([%p
                   Ast_builder.Default.pvar ~loc:field.loc
                     ("column_" ^ field.name)]
                :: columns) =
              columns
            in
            [%e e]])

    let expand_record ~context ~type_context (fields : Attributes.field list) =
      let module Ast = (val Ast_builder.make type_context.loc) in
      let loc = type_context.loc
      and column_name s = "column_" ^ s in
      let column_explosion_of_field (field : Attributes.field) =
        [%stri
          let Sqml.Expression.Column.List.(
                [%p Ast_builder.Default.pvar ~loc:field.loc field.name] :: rest)
              =
            rest]
      and column_of_field (field : Attributes.field) =
        let module Ast = (val Ast_builder.make field.loc) in
        let loc = field.loc in
        let column_type =
          match field.sqml_type with
          | Some t -> [%expr Atomic [%e t]]
          | None ->
            [%expr
              Schematic_sqml.type_of_schema
                [%e
                  Schema.(
                    schema_of_expanded ~context ~loc:field.loc
                    @@ encode_type ~context ~type_context field.type_)]]
        and fk =
          match field.sqml_foreign with
          | [] -> [%expr None]
          | keys -> [%expr Some [%e Ast.elist keys]]
        in
        [%stri
          let [%p Ast.pvar (column_name field.name)] =
            Sqml.Column.make_composite ?foreign_keys:[%e fk]
              [%e Ast.estring field.name] [%e column_type]]
      and column_type_of_field (field : Attributes.field) tail =
        [%type: [%t field.type_] -> [%t tail]]
      and expr_explosion_of_field (field : Attributes.field) =
        [%stri
          let Sqml.Expression.List.(
                [%p Ast_builder.Default.pvar ~loc:field.loc field.name] :: rest)
              =
            rest]
      and id_of_field (field : Attributes.field) =
        Ast_builder.Default.evar ~loc:field.loc (column_name field.name)
      and record_field_of_field (field : Attributes.field) =
        ( { txt = Lident field.name; loc = field.loc },
          Ast_builder.Default.evar ~loc:field.loc (column_name field.name) )
      and record_pattern_of_field (field : Attributes.field) =
        ( { txt = Lident field.name; loc = field.loc },
          Ast_builder.Default.pvar ~loc:field.loc (column_name field.name) )
      in
      let primary =
        List.filter fields ~f:(function
          | { sqml_primary = Some true; _ } -> true
          | _ -> false)
      in
      {
        columns = List.map ~f:column_of_field fields;
        column_explosions = List.map ~f:column_explosion_of_field fields;
        columns_list = Ast.elist (List.map ~f:id_of_field fields);
        columns_type =
          List.fold_right ~init:[%type: unit] ~f:column_type_of_field fields;
        fields;
        primary_type =
          List.fold_right ~init:[%type: unit] ~f:column_type_of_field primary;
        primary = Ast.elist (List.map ~f:id_of_field primary);
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

    let close ~configuration:name ~context:_ ~type_context
        {
          column_explosions;
          columns;
          columns_list;
          columns_type;
          expr_explosions;
          fields;
          primary_type;
          primary;
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
            [@@warning "-unused-type-declaration"]

            let columns = Sqml.Column.List.([%e columns_list])
            [@@warning "-unused-var-strict"]

            type primary = [%t primary_type]
            [@@warning "-unused-type-declaration"]

            let primary columns =
              let open Sqml.Expression.Column.List in
              [%e explode_columns ~loc fields [%expr [%e primary]]]
            [@@warning "-unused-var-strict"]
          end)

          type mapped =
            [%t Ast.ptyp_constr (Ast.Located.lident type_context.name) []]

          let map columns =
            let open Hlist in
            [%e explode_columns ~loc fields record]
          [@@warning "-unused-var-strict"]

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
end

let () = Plugin.register (module Sqml)

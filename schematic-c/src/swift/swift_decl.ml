open Base
open C_intf
open C_pp
open Swift_conv
open Swift_utils
open Swift_typeexpr

let case_decls basic rich =
  let open Fmt in
  let basic =
    let basic_case =
      let pp f id = Fmt.fmt "case %s" f (Swiftify.Binding.id id) in
      box pp
    in
    const (list ~sep:cut basic_case) basic
  in
  let rich =
    let rich_case =
      let pp f (id, c) =
        let swift_case = Swiftify.Binding.id id in
        match c with
        (* Inline tuples are special-cased to avoid extra parentheses *)
        | Atom (_, Some (Tuple _)) ->
          Fmt.fmt "case %s%a" f swift_case pp_swift_type c
        | Struct { fields } ->
          let named_member_decls =
            fields
            |> List.map ~f:(fun (id, c) ->
                   Fmt.str "%s: %a" (Swiftify.Binding.id id) pp_swift_type c)
            |> String.concat ~sep:", "
          in
          Fmt.fmt "case %s(%s)" f swift_case named_member_decls
        | _ -> Fmt.fmt "case %s(%a)" f swift_case pp_swift_type c
      in
      box pp
    in
    let pp = const (list ~sep:cut rich_case) rich in
    if List.is_empty rich then nop else cut ++ pp
  in
  basic ++ rich

let struct_field : (Identifier.t * c) Fmt.t =
  let open Fmt in
  let pp f (id, c) =
    Fmt.fmt "let %s: %a" f (Swiftify.Binding.id id) pp_swift_type c
  in
  box pp

(* Type definitions *)
let type_decl fmt (c_name, ts) =
  let qualified_c_name =
    Fmt.str "%s.%a" exported_module_name Identifier.pp c_name
  in
  let swift_type_name = Swiftify.Type.id c_name in
  let rec pp_swift_decl f ts =
    let printer =
      let open Fmt in
      let associated_typealias =
        const typealias (Identifier.to_string c_name)
      in
      let to_c = to_c ~c_type_name:(Identifier.to_string c_name) in
      let of_c =
        of_c ~c_type_name:(Identifier.to_string c_name) ~swift_type_name
      in
      (* Kinds of structures *)
      let struct_ : (Identifier.t * c) list Fmt.t =
       fun f fields ->
        let fields_decl = const (list ~sep:cut struct_field) fields in
        let to_c =
          let body =
            invoke_named qualified_c_name
              (fields
              |> List.map ~f:(fun (id, c) ->
                     let x =
                       to_c_stored ~c_type:c ~c_expr:(Swiftify.Binding.id id)
                     in
                     (Identifier.to_string id, const string x)))
          in
          to_c body
        in
        let of_c =
          let body =
            let inits =
              fields
              |> List.map ~f:(fun (id, c) ->
                     let x =
                       let c_expr =
                         Fmt.str "c.%s" (id |> Identifier.to_string)
                       in
                       of_c_stored ~c_type:c ~c_expr
                     in
                     (Swiftify.Binding.id id, const string x))
            in
            invoke_named swift_type_name inits
          in
          of_c body
        in
        let body =
          associated_typealias ++ cut ++ fields_decl ++ cut ++ to_c ++ cut
          ++ of_c
        in
        block ~indent
          (Fmt.str "public struct %s: CConvertible" swift_type_name)
          body f ()
      in
      let variant :
          (Identifier.t * Identifier.t list * (Identifier.t * c) list) Fmt.t =
       fun fmt (_, basic, rich) ->
        let all_basic = List.is_empty rich in
        let to_c =
          let basic =
            basic
            |> List.map ~f:(fun id ->
                   to_c_case ~all_basic ~c_variant_name:c_name
                     ~c_constructor_name:id ~c_constructor_type:None)
          in
          let rich =
            rich
            |> List.map ~f:(fun (id, c) ->
                   to_c_case ~all_basic ~c_variant_name:c_name
                     ~c_constructor_name:id ~c_constructor_type:(Some c))
          in
          basic @ rich |> switch "self" |> to_c
        in
        let of_c =
          let basic =
            basic
            |> List.map ~f:(fun id ->
                   of_c_case ~all_basic ~swift_type_name ~c_variant_name:c_name
                     ~c_constructor_name:id ~c_constructor_type:None)
          in
          let rich =
            rich
            |> List.map ~f:(fun (id, c) ->
                   of_c_case ~all_basic ~swift_type_name ~c_variant_name:c_name
                     ~c_constructor_name:id ~c_constructor_type:(Some c))
          in
          let default = const string {|fatalError("malformed C variant")|} in
          basic @ rich
          |> switch ~default (if all_basic then "c" else "c.tag")
          |> of_c
        in
        let body : unit Fmt.t =
          associated_typealias ++ cut ++ case_decls basic rich ++ cut ++ to_c
          ++ cut ++ of_c
        in
        block ~indent
          (Fmt.str "public enum %s: CConvertible" swift_type_name)
          body fmt ()
      in
      match ts with
      (* Structs and enums are named types *)
      | Struct { fields } -> const struct_ fields
      | Variant { name; basic; rich } -> const variant (name, basic, rich)
      | Poly_variant { name; basic; rich } ->
        let basic, rich = normalize_poly_variant_members basic rich in
        const variant (name, basic, rich)
      (* Mapped types delegate to their external representation *)
      | Mapped { front_facing } -> const pp_swift_decl front_facing
      | Void -> failwith "Tried to declare Void"
      (* Other types don't get their own declaration, and are just aliases *)
      | aliased ->
        const string
        @@ Fmt.str "public typealias %s = %a" swift_type_name pp_swift_type
             aliased
    in
    printer f ()
  in
  pp_swift_decl fmt ts

let type_decl (c_name, c) : string option =
  Logs.debug (fun m -> m "Encountering %a: %a" Identifier.pp c_name pp_c c);
  match c with
  (* Some types do not need to be individually declared *)
  | Tuple _ | Array _ -> None
  | c -> Some (Fmt.to_to_string type_decl (c_name, c))

open C_pp
open C_intf
open Base
open Swift_utils

let indent = 2

(* XXX visibility *)
let typealias f = Fmt.fmt "public typealias C = %s" f
let applied = block ~indent ~open_sp:false ~delim:('(', ')')

let vertical_comma_sep pp l =
  let open Fmt in
  const (list ~sep:(const char ',' ++ cut) pp) l

(* Function invocation / initializer call. *)
let invoke_named callee named_params =
  let open Fmt in
  let pp : (string * unit Fmt.t) Fmt.t =
   fun f (s, pp) -> box (Fmt.pair ~sep:(const string ": ") string pp) f (s, ())
  in
  applied callee (vertical_comma_sep pp named_params)

(* A Swift switch statement *)
let switch ?(default : unit Fmt.t option) (on : string)
    (cases : (string * unit Fmt.t) list) : unit Fmt.t =
  let open Fmt in
  let case_ish header body = vbox ~indent @@ (header ++ cut ++ body) in

  (* We don't use ablock: because we don't want to increase the indentation
     level inside the switch. *)
  let case : (string * unit Fmt.t) Fmt.t =
   fun f (case_match, case_body) ->
    let pp =
      case_ish (const string (Fmt.str "case %s:" case_match)) case_body
    in
    pp f ()
  in
  let cases : unit Fmt.t = const (list ~sep:cut case) cases in
  let maybe_default =
    match default with
    | Some body -> case_ish (const string "default:") body ++ cut
    | None -> nop
  in
  const string ("switch " ^ on ^ " {")
  ++ cut ++ cases ++ cut ++ maybe_default ++ const char '}'

let tuple_bindings n = List.init n ~f:(Fmt.str "t%d")

(* { return tuple_string_wrapper( t0: x.0.toC(), t1: x.1.toC() ) } *)

let rec pp_swift_type f c =
  Logs.debug (fun m -> m "pp_swift_type: %a" pp_c c);
  let open Fmt in
  let array : c Fmt.t = fun f -> Fmt.fmt "Array<%a>" f pp_swift_type in
  let pp =
    match c with
    (* Array<T> specialization. Aliases won't help with protocols so no use
       keeping them around on the Swift side. *)
    | Array t | Atom (_, Some (Array t)) -> const array t
    | String -> const string "String"
    (* Unlike C, Swift has native tuple support. Leverage that. *)
    | Tuple members | Atom (_, Some (Tuple members)) ->
      const char '('
      ++ const (list ~sep:(const string ", ") pp_swift_type) members
      ++ const char ')'
    | Atom (s, _) ->
      (* XXX this is ugly *)
      let override =
        match s |> Identifier.to_string with
        | "int64_t" -> Some "Int64"
        | "sch_json" -> Some "String"
        (* XXX not bijective *)
        | "sch_time" -> Some "SchematicTime"
        | _ -> None
      in
      const string (Option.value override ~default:(Swiftify.Type.id s))
    | Void -> const string "Void"
    | Mapped { front_facing } -> const pp_swift_type front_facing
    | Struct _ | Variant _ | Poly_variant _ ->
      failwith "tried to ref to %a without going through an Atom" pp_c c
  in
  pp f ()

let to_c_expr ~c_type ~c_local =
  (* TODO: handle unnamed types (which cannot be extended )*)
  match c_type with
  | Tuple _
  | Atom (_, Some (Tuple _))
  (* FIXME recursive traversal to find the hidden type *)
  | Atom (_, Some (Atom (_, Some (Tuple _)))) ->
    Fmt.str "tupleToC(%s)" c_local
  | x -> (
    let inner = Fmt.str "%s.toC()" c_local in
    match x with
    | Array c | Atom (_, Some (Array c)) ->
      Fmt.str "unsafeBitCast(%s, to: %a.self)" inner Identifier.pp
        (array_name c)
    | _ -> inner)

(* XXX/TODO: storage_requires_indirection? *)

(* Similar to to_c_expr, but takes into account constraints for storing `c_type`
   (i.e. need for indirection.) *)
let to_c_stored ~c_type ~c_local =
  let direct = to_c_expr ~c_type ~c_local in
  match c_type with
  (* XXX will hopefully remove soon *)
  (* | Struct _ | Atom (_, Some (Struct _)) -> Fmt.str "heapAlloc(%s)" direct *)
  | _ -> direct

(* TODO rename c_local c_expr *)
let of_c_expr ~c_type ~c_local =
  match c_type with
  (* Unnamed types cannot be extended *)
  | Tuple _
  | Atom (_, Some (Tuple _))
  (* FIXME recursive traversal to find the hidden type *)
  | Atom (_, Some (Atom (_, Some (Tuple _)))) ->
    Fmt.str "tupleOfC(%s)" c_local
  | c ->
    let inner =
      match c with
      | Array _ | Atom (_, Some (Array _)) ->
        Fmt.str "unsafeBitCast(%s, to: type_erased_array.self)" c_local
      | _ -> c_local
    in
    Fmt.str "%a.ofC(%s)" pp_swift_type c_type inner

let of_c_stored ~c_type ~c_local =
  let c_local =
    c_local
    ^
    match c_type with
    (* | Struct _ | Atom (_, Some (Struct _)) -> ".pointee" *)
    | _ -> ""
  in
  of_c_expr ~c_type ~c_local

let of_c ~c_type_name ~swift_type_name body =
  let of_c_decl =
    Fmt.str "public static func ofC(_ c: %a) -> %s" Identifier.pp c_type_name
      swift_type_name
  in
  block ~indent of_c_decl body

let to_c ~c_type_name body =
  let to_c_decl = Fmt.str "public func toC() -> %a" Identifier.pp c_type_name in
  block ~indent to_c_decl body

let tuple_conv_decl members : unit Fmt.t * unit Fmt.t =
  let open Fmt in
  let tname = tuple_name members |> Identifier.to_string in
  let bindings = tuple_bindings (List.length members) in
  let to_c =
    let signature =
      Fmt.str "func tupleToC(_ x: %a) -> %s" pp_swift_type (Tuple members) tname
    in
    let inits =
      List.zip_exn bindings members
      |> List.mapi ~f:(fun i (id, c) ->
             let c_local = Fmt.str "x.%d" i in
             let v = to_c_stored ~c_type:c ~c_local in
             (id, const string v))
    in
    let body = invoke_named tname inits in
    block ~indent signature body
  in
  let of_c =
    let signature =
      Fmt.str "func tupleOfC(_ c: %s) -> %a" tname pp_swift_type (Tuple members)
    in
    let body =
      let items =
        members
        |> List.mapi ~f:(fun i c ->
               let c_local = Fmt.str "c.t%d" i in
               of_c_stored ~c_type:c ~c_local)
      in
      const char '('
      ++ const (list ~sep:(const string ", ") string) items
      ++ const char ')'
    in
    block ~indent signature body
  in
  (of_c, to_c)

let to_c_case ~all_basic ~c_variant_name ~c_constructor_name
    ~(c_constructor_type : C_intf.c option) =
  let open Fmt in
  let c_case =
    C_intf.case_name ~rich:(not all_basic) c_variant_name c_constructor_name
    |> Identifier.to_string
  in
  let sw_id = Swiftify.Binding.id c_constructor_name in
  let case_header, payload =
    match c_constructor_type with
    | None -> ("." ^ sw_id, const string "nil")
    | Some c ->
      assert (not all_basic);
      let opaque_alloc x = applied "OpaquePointer" (applied "heapAlloc" x) in
      let bindings, body =
        match c with
        (* Inline tuple *)
        | Atom (c_tuple_type_name, Some (Tuple members)) ->
          let bindings = tuple_bindings (List.length members) in
          (* Tuple types CANNOT be extended in Swift. *)
          let tuple_construction =
            invoke_named
              (c_tuple_type_name |> Identifier.to_string)
              (List.zip_exn bindings members
              |> List.map ~f:(fun (name, c) ->
                     (name, const string (to_c_stored ~c_type:c ~c_local:name)))
              )
          in
          (bindings |> String.concat ~sep:", ", tuple_construction)
        (* Inline record *)
        | Struct { fields } ->
          let fields =
            fields |> List.map ~f:(fun (id, c) -> (Swiftify.Binding.id id, c))
          in
          let construction =
            invoke_named
              (inline_record_c_name
                 ~variant_name:(c_variant_name |> Identifier.to_string)
                 ~constructor_name:c_constructor_name)
              (fields
              |> List.map ~f:(fun (name, c) ->
                     (name, const string (to_c_stored ~c_type:c ~c_local:name)))
              )
          in
          let bindings = List.map fields ~f:(fun (name, _c) -> name) in
          (bindings |> String.concat ~sep:", ", construction)
        | _ -> ("x", const string (to_c_stored ~c_type:c ~c_local:"x"))
      in
      (Fmt.str "let .%s(%s)" sw_id bindings, opaque_alloc body)
  in

  let pp =
    if all_basic then const string @@ Fmt.str "return %s" c_case
    else
      const string "return "
      ++ invoke_named
           (c_variant_name |> Identifier.to_string)
           [ ("tag", const string c_case); ("payload", payload) ]
  in
  (case_header, pp)

let of_c_case ~all_basic ~swift_type_name ~c_variant_name ~c_constructor_name
    ~(c_constructor_type : C_intf.c option) =
  let open Fmt in
  let c_case =
    C_intf.case_name ~rich:(not all_basic) c_variant_name c_constructor_name
  in
  let sw_id = Swiftify.Binding.id c_constructor_name in
  let pp =
    let qualified_swift_constr = Fmt.str "%s.%s" swift_type_name sw_id in
    match c_constructor_type with
    | None -> const string ("return " ^ qualified_swift_constr)
    | Some c ->
      let payload_let pointee =
        let ptr_type = Fmt.str "UnsafePointer<%s>" pointee in
        const string (Fmt.str "let payload = %s(c.payload).pointee" ptr_type)
      in
      let pointee, intermediate_stmts, params =
        match c with
        (* Inline tuple *)
        | Atom (_, Some (Tuple members)) ->
          let bindings = tuple_bindings (List.length members) in
          let stmts =
            List.zip_exn bindings members
            |> List.map ~f:(fun (tx, c) ->
                   let c_local = Fmt.str "payload.%s" tx in
                   Fmt.str "let %s = %s" tx (of_c_stored ~c_type:c ~c_local))
          in
          let args = bindings |> String.concat ~sep:", " in
          (Swiftify.C_type.c c, stmts, args)
        (* Inline record *)
        | Struct { fields } ->
          let fields =
            fields |> List.map ~f:(fun (id, c) -> (Swiftify.Binding.id id, c))
          in
          let args =
            fields
            |> List.map ~f:(fun (label, c) ->
                   let c_local = Fmt.str "payload.%s" label in
                   let converted = of_c_expr ~c_type:c ~c_local in
                   Fmt.str "%s: %s" label converted)
          in
          ( inline_record_c_name
              ~variant_name:(Identifier.to_string c_variant_name)
              ~constructor_name:c_constructor_name,
            [],
            args |> String.concat ~sep:", " )
        | _ -> (Swiftify.C_type.c c, [], of_c_expr ~c_type:c ~c_local:"payload")
      in
      let inter =
        if List.is_empty intermediate_stmts then nop
        else const (list ~sep:cut string) intermediate_stmts ++ cut
      in
      payload_let pointee ++ cut ++ inter
      ++ const string (Fmt.str "return %s(%s)" qualified_swift_constr params)
  in
  (c_case |> Identifier.to_string, pp)

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

(* Type definitions *)
let type_decl fmt (c_name, ts) =
  let swift_type_name = Swiftify.Type.id c_name in
  let rec pp_swift_decl f ts =
    let printer =
      let open Fmt in
      let associated_typealias =
        const typealias (Identifier.to_string c_name)
      in
      let to_c = to_c ~c_type_name:c_name in
      let of_c = of_c ~c_type_name:c_name ~swift_type_name in
      (* Kinds of structures *)
      let struct_ : (Identifier.t * c) list Fmt.t =
       fun f fields ->
        let field : (Identifier.t * c) Fmt.t =
          let pp f (id, c) =
            Fmt.fmt "let %s: %a" f (Swiftify.Binding.id id) pp_swift_type c
          in
          box pp
        in
        let fields_decl = const (list ~sep:cut field) fields in
        (* FIXME: handling pointers coming from structs. And what about
           freeing? *)
        let to_c =
          let body =
            invoke_named
              (Identifier.to_string c_name)
              (fields
              |> List.map ~f:(fun (id, c) ->
                     let x =
                       to_c_stored ~c_type:c ~c_local:(Swiftify.Binding.id id)
                     in
                     (Identifier.to_string id, const string x)))
          in
          to_c body
        in
        let of_c =
          let body =
            invoke_named swift_type_name
              (fields
              |> List.map ~f:(fun (id, c) ->
                     let x =
                       let c_local =
                         Fmt.str "c.%s" (id |> Identifier.to_string)
                       in
                       of_c_stored ~c_type:c ~c_local
                     in
                     (Swiftify.Binding.id id, const string x)))
          in
          of_c body
        in
        (* FIXME extra leading spaces on blank lines... *)
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
          (* FIXME cuts *)
          associated_typealias ++ cut ++ case_decls basic rich ++ cut ++ to_c
          ++ cut ++ of_c
        in
        block ~indent
          (Fmt.str "public enum %s: CConvertible" swift_type_name)
          body fmt ()
      in
      match ts with
      (* Proper named types, getting their own declaration. *)
      | Struct { fields } -> const struct_ fields
      | Variant { name; basic; rich } -> const variant (name, basic, rich)
      | Poly_variant { name; basic; rich } ->
        let basic, rich = normalize_poly_variant_members basic rich in
        const variant (name, basic, rich)
      (* Special case: mapped types *)
      | Mapped { front_facing } -> const pp_swift_decl front_facing
      (* XXX (failwith?) *)
      | Void -> failwith "Void?"
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
  (* We do not re-declare built-in types *)
  (* | String | Tuple _ | Array _ -> None *)
  (* | Atom (_, Some (Tuple _ | Array _ | String)) -> None *)
  (* Tuple and array specializations *)
  | Tuple _ -> None
  | Array _ -> None
  | Mapped _ -> None
  | c -> Some (Fmt.to_to_string type_decl (c_name, c))

open C_pp
open C_intf
open Base
open Swift_utils

let indent = 2
let typealias f = Fmt.fmt "typealias C = %a" f Identifier.pp
let applied = braced ~indent ~open_sp:false ~delim:('(', ')')


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
let switch (on : string) (cases : (string * unit Fmt.t) list) : unit Fmt.t =
  let open Fmt in
  (* We don't use abraced: because we don't want to increase the indentation
     level inside the switch. *)
  let case : (string * unit Fmt.t) Fmt.t =
   fun f (case_match, case_body) ->
    let pp =
      vbox ~indent
      @@ (const string (Fmt.str "case %s:" case_match) ++ cut ++ case_body)
    in
    pp f ()
  in
  let cases : unit Fmt.t = const (list ~sep:cut case) cases in
  const string ("switch " ^ on ^ " {") ++ cut ++ cases ++ cut ++ const char '}'

let of_c ~c_type_name ~swift_type_name body =
  let of_c_decl =
    Fmt.str "public static func ofC(_ c: %a) -> %s" Identifier.pp c_type_name
      swift_type_name
  in
  braced ~indent of_c_decl body

let to_c ~c_type_name body =
  let to_c_decl = Fmt.str "public func toC() -> %a" Identifier.pp c_type_name in
  braced ~indent to_c_decl body

let tuple_bindings n = List.init n ~f:(Fmt.str "t%d")

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
              (bindings
              |> List.map ~f:(fun name ->
                     (name, const (Fmt.fmt "%s.toC()") name)))
          in
          (bindings |> String.concat ~sep:", ", tuple_construction)
        | _ -> ("x", const string "x.toC()")
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
      let payload_let =
        let ptr_type = Fmt.str "UnsafePointer<%s>" (Swiftify.C_type.c c) in
        const string (Fmt.str "let payload = %s(x.payload).pointee" ptr_type)
      in

      let intermediate_stmts, params =
        match c with
        (* Inline tuple *)
        | Atom (_, Some (Tuple members)) ->
          let bindings = tuple_bindings (List.length members) in
          let stmts =
            List.zip_exn bindings members
            |> List.map ~f:(fun (tx, c) ->
                   Fmt.str "let %s = %s.ofC(payload.%s)" tx (Swiftify.Type.c c)
                     tx)
          in
          let args = bindings |> String.concat ~sep:", " in
          (stmts, args)
        | _ -> ([], "payload.toC()")
      in
      let inter =
        if List.is_empty intermediate_stmts then nop
        else const (list ~sep:cut string) intermediate_stmts ++ cut
      in
      payload_let ++ cut ++ inter
      ++ const string (Fmt.str "return %s(%s)" qualified_swift_constr params)
  in

  (c_case |> Identifier.to_string, pp)

let case_decls pp_c basic rich =
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
        | Atom (_, Some (Tuple _)) -> Fmt.fmt "case %s%a" f swift_case pp_c c
        | _ -> Fmt.fmt "case %s(%a)" f swift_case pp_c c
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
  let rec pp f ts =
    let printer =
      let open Fmt in
      let typealias = const typealias c_name in
      let to_c = to_c ~c_type_name:c_name in
      let of_c = of_c ~c_type_name:c_name ~swift_type_name in
      (* Kinds of structures *)
      let array : c Fmt.t = fun f -> Fmt.fmt "Array<%a>" f pp in
      let struct_ : (Identifier.t * c) list Fmt.t =
       fun f fields ->
        let field : (Identifier.t * c) Fmt.t =
          let pp f (id, c) =
            Fmt.fmt "let %s: %a" f (Swiftify.Binding.id id) pp c
          in
          box pp
        in
        let fields_decl = const (list ~sep:cut field) fields in
      (* FIXME: handling pointers coming from structs. And what about freeing? *)
        let to_c =
          let body =
            invoke_named
              (Identifier.to_string c_name)
              (fields
              |> List.map ~f:(fun (id, _c) ->
                     ( Identifier.to_string id,
                       const (Fmt.fmt "%s.toC()") (Swiftify.Binding.id id) )))
          in
          to_c body
        in
        let of_c =
          let body =
            invoke_named swift_type_name
              (fields
              |> List.map ~f:(fun (id, c) ->
                     ( Swiftify.Binding.id id,
                       const string
                         (Fmt.str "%s.ofC(c.%a)" (Swiftify.Type.c c)
                            Identifier.pp id) )))
          in
          of_c body
        in
        (* FIXME extra leading spaces on blank lines... *)
        let body =
          typealias ++ cut ++ fields_decl ++ cut ++ to_c ++ cut ++ of_c
        in
        braced ~indent (Fmt.str "public struct %s" swift_type_name) body f ()
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
          basic @ rich |> switch (if all_basic then "c" else "c.tag") |> of_c
        in
        let body : unit Fmt.t =
          (* FIXME cuts *)
          typealias ++ cut ++ case_decls pp basic rich ++ cut ++ to_c ++ cut
          ++ of_c
        in
        braced ~indent (Fmt.str "public enum %s" swift_type_name) body fmt ()
      in
      match ts with
      (* Built-in Swift string type XXX test *)
      | String -> const string "String"
      (* Array<T> specialization XXX *)
      | Array t -> const array t
      | Struct { fields } -> const struct_ fields
      (* Unlike C, Swift has native tuple support. Leverage that. TODO: skip
         tuple instanciations *)
      | Atom (_, Some (Tuple _ as tup)) -> const pp tup
      (* Reference to another type *)
      | Atom (s, _) -> const string (Swiftify.Type.id s)
      (* FIXME tuple atom *)
      | Variant { name; basic; rich } -> const variant (name, basic, rich)
      | Poly_variant { name; basic; rich } ->
        let basic, rich = normalize_poly_variant_members basic rich in
        const variant (name, basic, rich)
      | Tuple members ->
        const char '(' ++ const (list ~sep:comma pp) members ++ const char ')'
      | Mapped { front_facing } -> const pp front_facing
      (* XXX (failwith?) *)
      | Void -> failwith "Void?"
    in
    printer f ()
  in
  pp fmt ts

let type_decl (c_name, c) : string option =
  match c with
  (* We do not re-declare built-in types *)
  | String | Tuple _ | Array _-> None
  | c -> Some (Fmt.to_to_string type_decl (c_name, c))

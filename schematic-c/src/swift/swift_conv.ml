open Base
open C_intf
open C_pp
open Swift_utils
open Swift_typeexpr

let to_c_expr ~c_type ~c_expr =
  if is_tuple c_type then Fmt.str "schematicToC(%s)" c_expr
  else
    let inner = Fmt.str "%s.toC()" c_expr in
    match c_type with
    | Array c | Atom (_, Some (Array c)) ->
      Fmt.str "unsafeBitCast(%s, to: %a.self)" inner Identifier.pp
        (array_name c)
    | _ -> inner

(* Similar to to_c_expr, but takes into account constraints for storing `c_type`
   (i.e. need for indirection.) *)
let to_c_stored ~c_type ~c_expr =
  (* Currently, no such constraint exists as we don't support recursive types. *)
  (*  Previously: "heapAlloc(%s)" *)
  to_c_expr ~c_type ~c_expr

let of_c_expr ~c_type ~c_expr =
  if is_tuple c_type then
    (* Unnamed types cannot be extended *)
    Fmt.str "schematicOfC(%s)" c_expr
  else
    let inner =
      if is_array c_type then
        Fmt.str "unsafeBitCast(%s, to: type_erased_array.self)" c_expr
      else c_expr
    in
    (* Using a top-level function implies more ambiguity, so we refer to the
       desired type explicitly whenever possible *)
    Fmt.str "%a.ofC(%s)" pp_swift_type c_type inner

let of_c_stored ~c_type ~c_expr =
  (* See `to_c_stored` comment *)
  of_c_expr ~c_type ~c_expr

let of_c ~c_type_name ~swift_type_name body =
  let of_c_decl =
    Fmt.str "public static func ofC(_ c: %a) -> %s" Identifier.pp c_type_name
      swift_type_name
  in
  block ~indent of_c_decl body

let to_c ~c_type_name body =
  let to_c_decl = Fmt.str "public func toC() -> %a" Identifier.pp c_type_name in
  block ~indent to_c_decl body

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
                     (name, const string (to_c_stored ~c_type:c ~c_expr:name)))
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
                     (name, const string (to_c_stored ~c_type:c ~c_expr:name)))
              )
          in
          let bindings = List.map fields ~f:(fun (name, _c) -> name) in
          (bindings |> String.concat ~sep:", ", construction)
        | _ -> ("x", const string (to_c_stored ~c_type:c ~c_expr:"x"))
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
                   let c_expr = Fmt.str "payload.%s" tx in
                   Fmt.str "let %s = %s" tx (of_c_stored ~c_type:c ~c_expr))
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
                   let c_expr = Fmt.str "payload.%s" label in
                   let converted = of_c_expr ~c_type:c ~c_expr in
                   Fmt.str "%s: %s" label converted)
          in
          ( inline_record_c_name
              ~variant_name:(Identifier.to_string c_variant_name)
              ~constructor_name:c_constructor_name,
            [],
            args |> String.concat ~sep:", " )
        | _ -> (Swiftify.C_type.c c, [], of_c_expr ~c_type:c ~c_expr:"payload")
      in
      let inter =
        if List.is_empty intermediate_stmts then nop
        else const (list ~sep:cut string) intermediate_stmts ++ cut
      in
      payload_let pointee ++ cut ++ inter
      ++ const string (Fmt.str "let res = %s(%s)" qualified_swift_constr params)
      ++ cut
      ++ const string "free(UnsafeMutableRawPointer(c.payload))"
      ++ cut ++ const string "return res"
  in
  (c_case |> Identifier.to_string, pp)

let tuple_conv_decl members : unit Fmt.t * unit Fmt.t =
  let open Fmt in
  let tname = tuple_name members |> Identifier.to_string in
  let bindings = tuple_bindings (List.length members) in
  let to_c =
    let signature =
      Fmt.str "func schematicToC(_ x: %a) -> %s" pp_swift_type (Tuple members)
        tname
    in
    let inits =
      List.zip_exn bindings members
      |> List.mapi ~f:(fun i (id, c) ->
             let c_expr = Fmt.str "x.%d" i in
             let v = to_c_stored ~c_type:c ~c_expr in
             (id, const string v))
    in
    let body = invoke_named tname inits in
    block ~indent signature body
  in
  let of_c =
    let signature =
      Fmt.str "func schematicOfC(_ c: %s) -> %a" tname pp_swift_type
        (Tuple members)
    in
    let body =
      let items =
        members
        |> List.mapi ~f:(fun i c ->
               let c_expr = Fmt.str "c.t%d" i in
               of_c_stored ~c_type:c ~c_expr)
      in
      const char '('
      ++ const (list ~sep:(const string ", ") string) items
      ++ const char ')'
    in
    block ~indent signature body
  in
  (of_c, to_c)

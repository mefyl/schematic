open Base
open Swift_utils
open C_intf

(* ref exposed for customizability *)
let c_module_name = Swift_utils.c_module_name

let type_decl (name, c) : string option =
  Logs.debug (fun m -> m "Swift type_decl: %a@." C_intf.pp_c c);
  Swift_decl.type_decl (name, c)

let prelude = [%file "Prelude.swift"]

let export_schemas schemas =
  let process_type (specialized, l) (id, c) =
    let decl = Option.value (type_decl (id, c)) ~default:"" in
    let f (pp_of, pp_to) flattened_c =
      let of_src, to_src =
        (Fmt.to_to_string pp_of (), Fmt.to_to_string pp_to ())
      in
      (Set.add specialized flattened_c, [ of_src; to_src ])
    in
    let specialized, extra_decls =
      match array_element c with
      | Some c
        when (not (Set.mem specialized (Array c))) && needs_toplevel_conv c ->
        f (Swift_conv.array_conv_decl c) (Array c)
      | _ -> (
        match tuple_members c with
        | Some members when not (Set.mem specialized (Tuple members)) ->
          f (Swift_conv.tuple_conv_decl members) (Tuple members)
        | _ -> (
          match option_element c with
          | Some c when not (Set.mem specialized (Option c)) ->
            f (Swift_conv.option_conv_decl c) (Option c)
          | _ -> (specialized, [])))
    in
    (specialized, extra_decls @ [ decl ] @ l)
  in
  let module C_for_set = struct
    module T = struct
      type t = C_intf.c [@@deriving ord]

      let sexp_of_t _ = Sexp.Atom "N/A"
    end

    include T
    include Comparable.Make (T)
  end in
  let user_dependent_code =
    let _, l =
      schemas |> C_schemas.of_schemas
      |> List.fold ~init:(Base.Set.empty (module C_for_set), []) ~f:process_type
    in
    l |> List.rev |> String.concat ~sep:"\n\n"
  in
  [prelude; Fmt.str "import %s" (!c_module_name); user_dependent_code] |>
  String.concat ~sep:"\n\n"

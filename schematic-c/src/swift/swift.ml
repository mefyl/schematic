open Base
open Swift_utils
open C_intf

let type_decl (name, c) : string option =
  Logs.debug (fun m -> m "Swift type_decl: %a@." C_intf.pp_c c);
  Swift_decl.type_decl (name, c)

let prelude = [%file "Prelude.swift"]

let export_schemas schemas =
  let process_type (specialized, l) (id, c) =
    let decl = Option.value (type_decl (id, c)) ~default:"" in
    let specialized, tuple_decls =
      match array_element c with
      | Some c -> failwith "TODO"
      | None -> (
        match tuple_members c with
        | Some members when not (Set.mem specialized (Tuple members)) ->
          let pp_of, pp_to = Swift_conv.tuple_conv_decl members in
          let of_src, to_src =
            (Fmt.to_to_string pp_of (), Fmt.to_to_string pp_to ())
          in
          (Set.add specialized (Tuple members), of_src ^ "\n\n" ^ to_src)
        | _ -> (specialized, ""))
    in
    (specialized, (tuple_decls ^ "\n\n" ^ decl) :: l)
  in
  let module C_for_set = struct
    module T = struct
      type t = C_intf.c [@@deriving ord, show]

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
    l |> String.concat ~sep:"\n\n"
  in
  prelude ^ "\n\n\n" ^ user_dependent_code

open Base

let type_decl (name, c) : string option =
  Logs.debug (fun m -> m "Swift type_decl: %a@." C_intf.pp_c c);
  Swift_typeexpr.type_decl (name, c)

let prelude = [%file "Prelude.swift"]

module C_tuple_members = struct
  module T = struct
    type t = C_intf.c list [@@deriving ord, show]

    let sexp_of_t _ = Sexp.Atom "N/A"
  end

  include T
  include Comparable.Make (T)

  type elem = t
end

let export_schemas schemas =
  let process_type (emitted_tuples, l) (id, c) =
    let decl = Option.value (type_decl (id, c)) ~default:"" in
    let emitted_tuples, tuple_decls =
      match c with
      | Tuple members | Atom (_, Some (Tuple members)) when not (Set.mem emitted_tuples members) ->
          let pp_of, pp_to = Swift_typeexpr.tuple_conv_decl members in
          let of_src, to_src =
            (Fmt.to_to_string pp_of (), Fmt.to_to_string pp_to ())
          in
          let emitted_tuples = Set.add emitted_tuples members in
          emitted_tuples, of_src ^ "\n\n" ^ to_src
      | _ -> emitted_tuples, ""
    in
    (emitted_tuples, (tuple_decls ^ "\n\n" ^ decl) :: l)
  in
  let user_dependent_code =
    let _, l =
      schemas |> C_schemas.of_schemas
      |> List.fold ~init:(Base.Set.empty (module C_tuple_members), []) ~f:process_type
    in
    l |> String.concat ~sep:"\n\n"
  in
  prelude ^ "\n\n\n" ^ user_dependent_code

open C_intf
open Base
open Swift_utils

let rec pp_swift_type f c =
  Logs.debug (fun m -> m "pp_swift_type: %a" pp_c c);
  let open Fmt in
  let array : c Fmt.t = fun f -> Fmt.fmt "Array<%a>" f pp_swift_type in
  let pp =
    match tuple_members c with
    | Some members ->
      const char '('
      ++ const (list ~sep:(const string ", ") pp_swift_type) members
      ++ const char ')'
    | None -> (
      match array_element c with
      | Some c -> const array c
      | None -> (
        match option_element c with
        | Some c -> const pp_swift_type c ++ const char '?'
        | None -> (
          match c with
          | String -> const string "String"
          | Atom (s, _) ->
            (* XXX this is ugly *)
            let override =
              match s |> Identifier.to_string with
              | "int64_t" -> Some "Int64"
              | "sch_json" -> Some "String"
              | "sch_time" -> Some "SchematicTime"
              | _ -> None
            in
            const string (Option.value override ~default:(Swiftify.Type.id s))
          | Void -> const string "Void"
          | Mapped { front_facing } -> const pp_swift_type front_facing
          | Struct _ | Variant _ | Poly_variant _ ->
            failwith "tried to ref to %a without going through an Atom" pp_c c
          | Tuple _ | Array _ | Option _ ->
            failwith
              "unreachable (if you do trigger this, time to try polyvars for \
               C_intf.c)")))
  in
  pp f ()

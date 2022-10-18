open Base
open C_intf

module C_type = struct
  (** Get the Swift name of a C type.

      It's _usually_ the same as the C type name, except for built-in C
      compatiblity types defined in the standard Swift distribution. *)
  let c c =
    let override =
      match c with
      | String -> Some "UnsafeMutablePointer<CChar>"
      | Atom (id, _) -> (
        match Identifier.to_string id with
        | "int" -> Some "CInt"
        | "bool" -> Some "CBool"
        | "double" -> Some "CDouble"
        | _ -> None)
      | _ -> None
    in
    match override with
    | Some s -> s
    | None -> c |> c_typename |> Identifier.to_string

  (* This is hackish. Always prefer `c` when possible.

     TODO allow renaming the Schematic built-ins. *)
  let s s =
    match s with
    | "string" -> c String
    | s -> c @@ Atom (Identifier.make s, None)
end

module Type = struct
  let s s =
    String.split s ~on:'_'
    |> List.map ~f:String.capitalize
    |> String.concat ~sep:""

  let id id = Identifier.to_string id |> s

  (* If you ever find yourself reaching for a [c] in this module, stop and use
     the recursive pp. *)
end

module Binding = struct
  let s = function
    | "self_" -> "self_"
    | s -> (
      match String.split s ~on:'_' with
      | h :: t ->
        let t = List.map ~f:String.capitalize t in
        String.concat ~sep:"" (h :: t)
      | [] -> "")

  let id id = Identifier.to_string id |> s
end

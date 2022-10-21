open Base
open C_intf

module C_type = struct
  (** Get the Swift name of a C type.

      It's the same as the C type name, except for built-in C compatiblity types
      defined in the standard Swift distribution. *)
  let c c =
    let override =
      match c with
      | `String -> Some "UnsafePointer<CChar>"
      | `Atom (id, _) -> (
        match Identifier.to_string id with
        | "int" -> Some "CInt"
        | "long" -> Some "CLong"
        | "bool" -> Some "CBool"
        | "double" -> Some "CDouble"
        | "int32_t" -> Some "Int32"
        | "int64_t" -> Some "Int64"
        | _ -> None)
      | _ -> None
    in
    match override with
    | Some s -> s
    | None -> c |> c_typename |> Identifier.to_string
end

module Type = struct
  let s s =
    String.split s ~on:'_'
    |> List.map ~f:String.capitalize
    |> String.concat ~sep:""

  let id id = Identifier.to_string id |> s

  (* If you ever find yourself reaching for a [c] in this module, stop and use
     [pp_swift_type]. *)
end

module Binding = struct
  let s = function
    (* [self] is a reserved keyword in Swift. *)
    | "self_" -> "self_"
    | s -> (
      match String.split s ~on:'_' with
      | h :: t ->
        let t = List.map ~f:String.capitalize t in
        String.concat ~sep:"" (h :: t)
      | [] -> "")

  let id id = Identifier.to_string id |> s
end

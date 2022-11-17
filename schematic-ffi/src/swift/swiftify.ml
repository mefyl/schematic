open Base
open C_intf

module C_type = struct
  (** Get the Swift name of a C type.

      It's the same as the C type name, except for built-in C compatiblity types
      defined in the standard Swift distribution. *)
  let c = function
    | `String -> "UnsafePointer<CChar>"
    | `Int -> "CInt"
    | `Bool -> "CBool"
    | `Double -> "CDouble"
    | `Int64 -> "Int64"
    | c -> c |> c_typename |> Identifier.to_string
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
  let reserved =
    [
      "as";
      "case";
      "class";
      "default";
      "defer";
      "deinit";
      "do";
      "else";
      "enum";
      "extension";
      "fallthrough";
      "false";
      "func";
      "guard";
      "init";
      "nil";
      "out";
      "raise";
      "self";
      "struct";
      "super";
      "switch";
      "throw";
      "true";
    ]

  let s s =
    (* camelCase *)
    let s =
      match String.split s ~on:'_' with
      | h :: t ->
        let t = List.map ~f:String.capitalize t in
        String.concat ~sep:"" (h :: t)
      | [] -> ""
    in
    if List.mem ~equal:String.equal reserved s then s ^ "_" else s

  let id id = Identifier.to_string id |> s
end

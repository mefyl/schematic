open Base
open C_intf
open C_pp

let indent = 2

module Swiftify = struct
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
    let s s =
      match String.split s ~on:'_' with
      | h :: t ->
        let t = List.map ~f:String.capitalize t in
        String.concat ~sep:"" (h :: t)
      | [] -> ""

    let id id = Identifier.to_string id |> s
  end
end

let rec tuple_members = function
  | Atom (_, None) -> None
  | Atom (_, Some c) -> tuple_members c
  | Tuple members -> Some members
  | _ -> None

let is_tuple = Fn.compose Option.is_some tuple_members

let rec array_element = function
  | Atom (_, None) -> None
  | Atom (_, Some c) -> array_element c
  | Array c -> Some c
  | _ -> None

let is_array = Fn.compose Option.is_some array_element
let tuple_bindings n = List.init n ~f:(Fmt.str "t%d")
let typealias f = Fmt.fmt "public typealias C = %s" f

let applied opening body =
  block ~indent ~open_sp:false ~delim:('(', ')') opening body

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
  (* We don't use a block because we don't want to increase the indentation
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

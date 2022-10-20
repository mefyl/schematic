open Base
open C_intf
open C_pp

let c_module_name = ref "Schemas"
let indent = 2

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

let rec option_element = function
  | Atom (_, None) -> None
  | Atom (_, Some c) -> option_element c
  | Option c -> Some c
  | _ -> None

let rec is_option = Fn.compose Option.is_some option_element
let is_array = Fn.compose Option.is_some array_element

let rec needs_toplevel_conv c =
  match array_element c with
  | Some c -> needs_toplevel_conv c
  | None -> is_tuple c || is_option c

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

let qualified_c_type ~c_type_name = Fmt.str "%s.%s" !c_module_name c_type_name

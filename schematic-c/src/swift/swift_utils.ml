open Base
open C_intf
open C_pp

let c_module_name = ref "Schemas"
let indent = 2
let first_some_l = List.fold ~f:Option.first_some ~init:None

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
  (* We don't use a block because we don't want to increase the indentation
     level inside the switch. *)
  let case : (string * unit Fmt.t) Fmt.t =
   fun f (case_match, case_body) ->
    let pp =
      case_ish ~indent (const string (Fmt.str "case %s:" case_match)) case_body
    in
    pp f ()
  in
  let cases : unit Fmt.t = const (list ~sep:cut case) cases in
  let maybe_default =
    match default with
    | Some body -> case_ish ~indent (const string "default:") body ++ cut
    | None -> nop
  in
  const string ("switch " ^ on ^ " {")
  ++ cut ++ cases ++ cut ++ maybe_default ++ const char '}'

let qualified_c_type ~c_type_name = Fmt.str "%s.%s" !c_module_name c_type_name
let swift_statements l = Fmt.(const (list string ~sep:cut) l)

open Base
include Ffi_format
open C_format

let tuple_bindings n = List.init n ~f:(Fmt.str "t%d")
let typealias f = Fmt.fmt "public typealias C = %s" f

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
  ++ cut
  ++ cases
  ++ cut
  ++ maybe_default
  ++ const char '}'

let swift_statements l = Fmt.(const (list string ~sep:cut) l)

let closure args body =
  let open Fmt in
  let pp_arg : _ Fmt.t =
   fun f arg ->
    match arg with
    | `Auto arg -> string f arg
    | `Typed (arg, typ) -> Fmt.fmt "(%s: %s)" f arg typ
  in
  let maybe_in = if List.is_empty args then nop else const string " in" in
  let args = const (list pp_arg ~sep:(const char ',')) args in
  vbox ~indent (const string "{ " ++ args ++ maybe_in ++ cut ++ vbox body)
  ++ cut
  ++ const char '}'

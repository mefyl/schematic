open Base
open C_intf
include Swift_format

(* FIXME *)
let indent = 2
let c_module_name = ref None

let qualified_c_type ~c_type_name =
  match !c_module_name with
  | Some m -> Fmt.str "%s.%s" m c_type_name
  | None -> c_type_name

let first_some_l = List.fold ~f:Option.first_some ~init:None

let rec needs_toplevel_conv c =
  match array_element c with
  | Some c -> needs_toplevel_conv c
  | None -> is_tuple c || is_option c

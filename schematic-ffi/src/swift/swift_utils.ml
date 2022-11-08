open Base
open C_intf
include Swift_format

(* FIXME *)
let indent = 2

let c_module_name = ref "Schemas"
let qualified_c_type ~c_type_name = Fmt.str "%s.%s" !c_module_name c_type_name

let first_some_l = List.fold ~f:Option.first_some ~init:None

let rec needs_toplevel_conv c =
  match array_element c with
  | Some c -> needs_toplevel_conv c
  | None -> is_tuple c || is_option c



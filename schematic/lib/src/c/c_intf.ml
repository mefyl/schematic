type t =
  | Void  (** Zero-Sized Type *)
  | String  (** Native string *)
  | Array of t
  | Atom of string * bool  (** Reference to another type. RHS : is_ptr *)
  | Struct of { fields : (string * t) list }
  | Variant of {
      name : string;
      basic : string list;
      rich : (string * t) list;
    }
  | Tuple of t list
[@@deriving show]

type c = t

(* XXX: order of signatures! *)

type c_function = {
  signature : string;
  impl : unit Fmt.t;
}

type c_gen = {
  name : string;
  type_expr : unit Fmt.t;
  to_caml : c_function;
  of_caml : c_function; (* TODO free *)
}

open Base

(* XXX: tuples of tuples - name conflicts *)

let rec c_typename = function
  | String -> "string"
  | Atom (s, _) -> s
  | Tuple members -> tuple_name members
  | Array t -> array_name t
  | x -> failwith "unsupported: %a" pp x

and tuple_name items =
  "tuple_"
  ^ (items
   (* FIXME: use typename *)
   |> List.map ~f:c_typename
    |> String.concat ~sep:"_")

and array_name t = c_typename t ^ "_array"

let tuple_fields = List.mapi ~f:(fun i c -> (Fmt.str "t%d" i, c))
let of_caml_name_for = Fmt.str "%s_of_caml"
let to_caml_name_for = Fmt.str "%s_to_caml"

(* FIXME: this approach does not protect against collisions *)
let of_caml_name_for_c = Fn.compose of_caml_name_for c_typename
let to_caml_name_for_c = Fn.compose to_caml_name_for c_typename

let case_name variant_name constr_name =
  Fmt.str "%s_%s_tag" variant_name constr_name

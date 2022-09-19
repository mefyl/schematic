type t =
  | Void
  | Atom of string
  | Pointer of t
  | Struct of { fields : (string * t) list }
  | Variant of (string * (string * t) list)
      (**(variant name (for prefixes)) X (constructor name X constructor
         contents) *)
  | Tuple of t list
[@@deriving show]

type c = t

open Base

(** Separate a variant's basic (LHS) and rich (RHS) constructors *)
let basic_rich_split =
  List.partition_map ~f:(function
    | (_, Void) as x -> First x
    | (_, _) as x -> Second x)

let tuple_name items =
  "tuple_"
  ^ (items
    |> List.map ~f:(function
         | Atom s -> s
         | Pointer (Atom s) -> s ^ "_ptr"
         | _ -> failwith "non-atom tuple member?")
    |> String.concat ~sep:"_")

let tuple_fields = List.mapi ~f:(fun i c -> (Fmt.str "t%d" i, c))
let of_caml_name_for = Fmt.str "%s_of_caml"
let to_caml_name_for = Fmt.str "%s_to_caml"

let c_typename = function
  | Pointer (Atom "char") -> "string"
  | Atom s -> s
  | Pointer (Atom s) ->
    s (* XXX! no distinction between a type and a pointer to it *)
  | Tuple members -> tuple_name members
  | x -> failwith "unsupported: %a" pp x

let of_caml_name_for_c = Fn.compose of_caml_name_for c_typename
let to_caml_name_for_c = Fn.compose to_caml_name_for c_typename

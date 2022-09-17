type t =
  | Void
  | Atom of string
  | Pointer of t
  (* TODO remove case, fix walker *)
  | Case of string * t
  | Struct of { fields : (string * t) list }
  | Variant of (string * (string * t) list)
      (**(variant name (for prefixes)) X (constructor name X constructor
         contents) *)
[@@deriving show]

type c = t

open Base

(** Separate a variant's basic (LHS) and rich (RHS) constructors *)
let basic_rich_split =
  List.partition_map ~f:(function
    | (_, Void) as x -> First x
    | (_, _) as x -> Second x)

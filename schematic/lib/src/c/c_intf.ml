type t =
  | Atom of string
  | Pointer of t
  | Struct of { fields : (string * t) list }
[@@deriving show]

type c = t

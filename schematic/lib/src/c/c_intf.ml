type t =
  | Atom of string
  | Struct of { fields : (string * t) list }
[@@deriving show]

type c = t

type t =
  | Atom of string
  | Constant of string
  | Case of string * t
  | Pointer of t
  | Struct of { fields : (string * t) list }
  | Variant of (string * (string * t) list)
      (**(variant name (for prefixes)) X (constructor name X constructor
         contents) *)
[@@deriving show]

type c = t

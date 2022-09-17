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

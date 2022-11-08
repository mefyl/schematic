open Base

type c_leaf =
  [ `Void
  | `Unit
  | `String
  | `Bool
  | `Int
  | `Int64
  | `Double
  | `Time
  | `Json
  ]
[@@deriving show, hash, eq, ord]

[@@@warning "-30"]

type regular_variant = {
  name : Identifier.t;
  basic : Identifier.t list;
  rich : (Identifier.t * c) list;
}

and poly_variant = {
  name : Identifier.t;
  (* int: hash *)
  basic : (Identifier.t * int) list;
  rich : (Identifier.t * int * c) list;
}

and c_struct = { fields : (Identifier.t * c) list }

and c =
  [ c_leaf
  | `Array of c
  | `Atom of Identifier.t * c option
    (** The RHS is the referenced type, if known. *)
  | `Struct of c_struct
  | `Variant of regular_variant
  | `Poly_variant of poly_variant
  | `Tuple of c list
  | `Mapped of c
  | `Option of c
  ]
[@@deriving show, hash, eq, ord]

[@@@warning "+30"]

type c_function = {
  signature : string;
  impl : unit Fmt.t;
}

type c_gen = {
  name : Identifier.t;
  type_expr : unit Fmt.t;
  to_caml : c_function;
  of_caml : c_function;
}

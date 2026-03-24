type token =
  | End
  | Null
  | Bool of bool
  | String of string
  | Float of float
  | Name of string
  | Array_start
  | Array_end
  | Object_start
  | Object_end
[@@deriving show]

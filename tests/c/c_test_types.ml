open Base

module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

type primitive_record = {
  a : int;
  b : int;
}
[@@deriving schema, show, eq]

type wrapper = {
  somevar : int;
  r : primitive_record;
}
[@@deriving schema, show, eq]

type type_alias = int [@@deriving schema, show, eq]
type string_wrapper = string [@@deriving schema, show, eq]
type diverse = { s : string } [@@deriving schema, show, eq] [@@boxed]

(* A variant with no value inside. *)
type basic_variant =
  | A
  | B
[@@deriving schema, show, eq]

type payload_variant =
  | Nothing
  | SomeInt of int
[@@deriving schema, show, eq]

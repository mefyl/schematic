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
  | Nothing2
  | SomeInt of int
  | Nothing3
[@@deriving schema, show, eq]

type pair_alias = string * int [@@deriving schema, show, eq]
type collection = int list [@@deriving schema, show, eq]

type behemoth = {
  d : diverse;
  b : basic_variant;
  p : payload_variant;
  pair : string * wrapper;
  pair_alias : pair_alias;
}
[@@deriving schema, show, eq]

type record_with_pair_alias = {
  a : int;
  pair_alias : pair_alias;
}
[@@deriving schema, show, eq]

type record_with_variant = { v : payload_variant } [@@deriving schema, show, eq]

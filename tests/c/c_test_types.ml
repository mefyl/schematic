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
  | A_pair of int * string
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

(* Roundtrip only *)

type int64_alias = int64 [@@deriving schema, show, eq]
type bool_alias = bool [@@deriving schema, show, eq]
type float_alias = float [@@deriving schema, show, eq]
type int_option_alias = int option [@@deriving schema, show, eq]
type float_map_alias = (string * float) list [@@deriving schema, show, eq]
type indirected_record_1 = primitive_record [@@deriving schema, show, eq]
type indirected_record_2 = indirected_record_1 [@@deriving schema, show, eq]

type record_with_indir_field = { indir : indirected_record_2 }
[@@boxed] [@@deriving schema, show, eq]

type problem_child = { only_field : string } [@@deriving schema, show, eq]

type record_containing_ambiguous_record = {
  problem_child : problem_child;
  padding_field : int;
}
[@@deriving schema, show, eq]

type any_alias = Schematic.Schemas.any [@@deriving schema, eq]

let pp_any_alias : any_alias Fmt.t = Fmt.of_to_string Ezjsonm.get_string

type any_manual_annotation =
  (Ezjsonm.value[@schematic.schema fun _ -> Schematic.Schemas.any_schema])
[@@deriving schema]

let pp_any_manual_annotation : any_manual_annotation Fmt.t =
  Fmt.of_to_string Ezjsonm.get_string

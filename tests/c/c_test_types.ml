open Base
open Schematic.Schemas

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

type string_option_record = { maybe : string_wrapper option }
[@@deriving schema, show, eq]

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

type time_alias = Ptime.t [@@deriving schema, show, eq]
type time_local_alias = Ptime.local [@@deriving schema, show, eq]
type date_alias = Ptime.date [@@deriving schema, eq]

type 'a parametric_record = { parameterized_field : 'a }
[@@boxed] [@@deriving schema, eq, show]

type record_with_unit = {
  concrete : int;
  nothing : unit;
  after_unit : int;
}
[@@deriving schema, show, eq]

type variant_with_inline_record_payload =
  | Payloads_are_overrated_anyway
  | Inline_payload of { field : int }
[@@deriving schema, show, eq]

(* Purposeful name conflict *)
type time = Ptime.t [@@deriving schema, show, eq]

type payload_variant_with_union_key =
  | Nothing_uk
  | A_pair_uk of int * int
  | Nothing2_uk
[@@deriving schema, show, eq] [@@schematic.union_key "type"]

module Weekdays = struct
  type t = {
    monday : bool; [@default false]
    tuesday : bool; [@default false]
    wednesday : bool; [@default false]
    thursday : bool; [@default false]
    friday : bool; [@default false]
    saturday : bool; [@default false]
    sunday : bool; [@default false]
  }
  [@@deriving eq, show, schema]
end

type proto_rule =
  | Week of {
      (* TODO: investigate why schematic.default has such far-reaching impact.
         Maybe we should preprocess the schemas first by removing anything
         'special'... *)
      period : int; [@schematic.default 1]
      weekdays : Weekdays.t option;
    }
[@@deriving eq, show, schema]

type postpone = { b : bool [@default false] }
[@@deriving eq, show, schema] [@@boxed]

type poly_basic =
  [ `A
  | `B
  | `C
  ]
[@@deriving eq, show, schema]

type poly_payload =
  [ `A
  | `Single of int
  | `B
  | `Tuple of int * string
  ]
[@@deriving eq, show, schema]

type evil_defaults = { not_option : int [@default 123] }
[@@deriving eq, show, schema, make] [@@boxed]

(* Recursive type *)
(* type node = | Nil | Cons of node [@@deriving schema, eq, show] *)

(* Nested generics *)
type ('coin, 'elem, 'a) ultra_generic = {
  field : 'coin;
  list : 'elem list;
  nested_generic : 'a parametric_record;
}
[@@deriving schema, show, eq]

type reified_parametric_record = string_wrapper parametric_record

let reified_parametric_record_schema =
  parametric_record_schema string_wrapper_schema

type reified_ultra_generic =
  (int, string parametric_record, string) ultra_generic

let reified_ultra_generic_schema : reified_ultra_generic Schema.t =
  ultra_generic_schema int_schema reified_parametric_record_schema string_schema

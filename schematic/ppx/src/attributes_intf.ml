open Base
open Ppxlib

module Source = struct
  (** Where a configuration item came from: generic attributes versus schematic
      specific (eg. \@default vs \@schematic.default) *)
  type 'a t =
    | Generic of 'a
    | Schematic of 'a

  let value = function Generic v | Schematic v -> v
  let generic v = Generic v
  let schematic v = Schematic v

  let merge ~loc name l r =
    match (l, r) with
    | None, v -> v
    | Some (Generic _), (Schematic _ as v) -> v
    | Some (Schematic _ as v), Generic _ -> v
    | _ ->
      Ppxlib.Location.raise_errorf ~loc
        "schematic-ppx: duplicate value for attribute %S" name
end

type inline =
  | Inline
  | Inline_try
  | Outline

type specs = {
  default : expression Source.t option;
  description : string option;
  examples : expression list;
  maximum : int option;
  minimum : int option;
  omit : bool; [@default false]
}
[@@deriving make]

type 'a versioned =
  ('a * (Semver.t * [ `Eq | `Gt | `Gte | `Lt | `Lte ]) option) Ast.loc list

type field = {
  alias : string versioned; [@default []]
  computed : expression option;
  inline : inline option;
  key : bool; [@default false]
      (** [\@schematic.key] attribute: mark this field as a discriminant of the
          enclosing variant case. *)
  loc : location;
  mutable_ : bool; [@default false]
  name : string; [@main]
  plugins : Hmap.t; [@default Hmap.empty]
  schema : expression versioned; [@default []]
  since : Semver.t option;
  specs : specs; [@default make_specs ()]
  type_ : core_type;
}
[@@deriving make]

type requirement =
  | Computed
  | Computed_optional
  | Default_opt of expression
  | Optional
  | Required

let check_unknown_attribute ~loc name =
  match String.chop_prefix ~prefix:"schematic." name with
  | Some name ->
    Ppxlib.Location.raise_errorf ~loc
      "schematic-ppx: unknown schematic annotation %s" name
  | None -> ()

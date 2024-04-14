open Ppxlib

(** Global settings of the ppx deriver. *)
type global_context = {
  enable_jsoo : bool;  (** Whether to output JSON types. *)
  inside : bool;  (** Whether we're inside schematic itself. *)
  version : Semver.t option;  (** The API version the user asked for. *)
}

(** Context for the types declaration being derived. *)
type context = {
  code_path : Code_path.t;  (** Code path of the types declaration. *)
  fixpoint : bool;
      (** Whether to force declaration as a fixpoint, even if the type is
          parametric. *)
  gadt_variant : string option;
      (** Which GADT variant this derivation is for, if any. *)
  global : global_context;  (** The global settings. *)
  loc : location;  (** AST location of the types declaration. *)
  recursive : bool;  (** Whether this type declaration is recursive. *)
  types : string list;  (** Name of all the types being declared. *)
}

(** Context for the derivation of an individual type. *)
type type_context = {
  attributes : Attributes.t;
      (** Attributes that were specified on the derived type. *)
  loc : location;  (** Location of the derived type. *)
  name : string;  (** Name of the derived type. *)
  params : (core_type * (variance * injectivity)) list;
      (** Parameters of the derived type. *)
}

and type_ =
  | Abstract
  | Core of core_type
  | Record of Attributes.field list
  | Variant of constructor_declaration list

open Ppxlib

type context = {
  code_path : Code_path.t;
  fixpoint : bool;
  gadt_variant : string option;
  global : global_context;
  loc : location;
  recursive : bool;
  types : string list;
}

and global_context = {
  enable_jsoo : bool;
  inside : bool;
  version : Semver.t option;
}

type type_context = {
  attributes : Attributes.t;
  loc : location;
  name : string;
  params : (core_type * (variance * injectivity)) list; (* type_ : string; *)
}

and type_ =
  | Abstract
  | Core of core_type
  | Record of Attributes.field list
  | Variant of constructor_declaration list

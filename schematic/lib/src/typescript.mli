(** The deduced typescript typing for a schema. *)
type t

(** [of_descriptor s] is the typescript type corresponding to descriptor [d]. *)
val of_descriptor : 'a Schema.descriptor -> t

(** [of_schema s] are the typescript types corresponding to schema [s] and all
    referenced schemas. *)
val of_schema : 'a Schema.t -> (string * t) list

(** [name t] is the typescript name of the type mapping to the type named [t] in
    OCaml. *)
val name : string -> string

(** [pp fmt t] outputs typescript types declaration for [t] to [fmt]. *)
val pp : ?indent:int -> Format.formatter -> t -> unit

(** [to_string t] is the typescript type declarations for [t]. *)
val to_string : t -> string

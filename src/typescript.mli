(** The deduced typescript typing for a schema. *)
type t

(** [of_schema s] is the typescript typing corresponding to schema [s]. *)
val of_schema : 'a Schema.t -> t

(** [name t] is the typescript name of the type mapping to the type named [t] in
    OCaml. *)
val name : string -> string

(** [pp fmt t] outputs typescript types declaration for [t] to [fmt]. *)
val pp : ?indent:int -> Format.formatter -> t -> unit

(** [to_string t] is the typescript type declarations for [t]. *)
val to_string : t -> string

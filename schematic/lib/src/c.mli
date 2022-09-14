(** The C struct typedef corresponding to a given schema. *)
type t

val to_string : t -> string

(** [of_schema s] are the C types corresponding to schema [s] and all referenced
    schemas. *)
val of_schema : 'a Schema.t -> (string * t) list

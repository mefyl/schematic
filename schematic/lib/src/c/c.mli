(** The C struct typedef corresponding to a given schema. *)
type t

val to_c_type_expression : t -> string

(** [of_schema s] are the C types corresponding to schema [s] and all referenced
    schemas. *)
val of_schema : 'a Schema.t -> (string * t) list

val to_of_caml_src : string -> t -> string
val to_to_caml_src : string -> t -> string

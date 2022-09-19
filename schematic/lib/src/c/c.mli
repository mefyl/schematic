(** The C struct typedef corresponding to a given schema. *)
type t

val to_c_type_expression : t -> string

(** [of_schema s] are the C types corresponding to schema [s] and all referenced
    schemas. *)
val of_schema : 'a Schema.t -> (string * t) list

(** A state to be passed across invocation to avoid emitting types multiple
    times. *)
type state

val state_empty : state

(** [of_schema_state state s] are the C types corresponding to schema [s] and
    all referenced schemas. Schemas already present in [state] are not emitted
    again, and an updated state with newly emitted types is returned. *)
val of_schemas_state : state -> Schema.pack list -> (string * t) list * state

val to_of_caml_src : string -> t -> string
val to_to_caml_src : string -> t -> string

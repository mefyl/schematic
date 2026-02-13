(** The deduced typescript typing for a schema. *)
type t

type documented = {
  type_ : t;
  documentation : string option;
}

(** [of_descriptor s] is the typescript type corresponding to descriptor [d]. *)
val of_descriptor : 'a Schema.descriptor -> t

(** [of_schema s] are the typescript types corresponding to schema [s] and all
    referenced schemas, with optional documentation. *)
val of_schema : 'a Schema.t -> (string * documented) list

(** [of_schemas ?exclude schemas] are the typescript types corresponding to all
    schemas in [schemas] and all referenced schemas, except the one listed in
    [excluded], with optional documentation. *)
val of_schemas :
  ?exclude:Schema.pack list -> Schema.pack list -> (string * documented) list

(** [name t] is the typescript name of the type mapping to the type named [t] in
    OCaml. *)
val name : string -> string

(** [pp fmt t] outputs typescript types declaration for [t] to [fmt].

    - [annotate_since] preserves [@schematic.since] annotation in comments. *)
val pp : ?annotate_since:bool -> ?indent:int -> Format.formatter -> t -> unit

(** [to_string ?annotate_since ?indent t] is the typescript type declarations
    for [t].

    - Add version comments iff [annotate_since]. *)
val to_string : ?annotate_since:bool -> ?indent:int -> t -> string

(** [pp_toplevel fmt (name, (t, doc))] prints a toplevel TypeScript type
    declaration with optional JSDoc documentation. *)
val pp_toplevel :
  ?annotate_since:bool ->
  ?indent:int ->
  Format.formatter ->
  string * documented ->
  unit

(** A state to be passed across invocation to avoid emitting types multiple
    times. *)
type state

(** Initial empty state *)
val state_empty : state

(** State that prevent given types from being emitted *)
val state_exclude : Schema.pack list -> state

(** [of_schema_state state s] are the typescript types corresponding to schema
    [s] and all referenced schemas. Schemas already present in [state] are not
    emitted again, and an updated state with newly emitted types is returned. *)
val of_schemas_state :
  state -> Schema.pack list -> (string * documented) list * state

include module type of Json_schema_intf

val make :
  ?applicator:applicator ->
  ?core:core ->
  ?metadata:metadata ->
  ?validation:validation ->
  unit ->
  t

(** [of_schema ?reference schema] is the JSON schema for schematic schema
    [schema]. If [?reference] is [None] or [Some `Ids], objects have an ["$id"]
    field set from the schema name, and are referenced through their id. If
    [?reference] is [Some `Defs], the ["$defs"] dictionary is populated from all
    referenced objects, the ["$id"] field is absent, and objects are referenced
    through ["#/$defs"].

    The [?reference] parameter is there to allow working with OpenAI, but should
    be revisited at some point to have a better interface. *)
val of_schema : ?reference:[ `Defs | `Ids ] -> _ Schema.t -> t

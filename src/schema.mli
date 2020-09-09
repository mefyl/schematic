include module type of Schema_intf

val map : ('a -> 'b) -> ('b -> 'a) -> 'a schema -> 'b schema

val pp : Format.formatter -> _ schema -> unit

val extract_field_schema : 'a field_schema -> bool * 'a schema

include module type of Schema_intf

val map : ('a -> 'b) -> ('b -> 'a) -> 'a schema -> 'b schema

val pp : Format.formatter -> _ schema -> unit

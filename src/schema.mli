include module type of Schema_intf

val required : (_, _) field_requirement -> bool

val map : ('a -> 'b) -> ('b -> 'a) -> 'a schema -> 'b schema

val pp : Format.formatter -> _ schema -> unit

val pp_descriptor : Format.formatter -> (_, _) descriptor -> unit

val extract_field_schema : 'a field_schema -> bool * 'a schema

val descriptor_fields : ('a, [ `Value ]) descriptor -> string list

type 'res inline = { f : 't. 't field -> 't -> 'res }

val inline : ('t, [ `Value ]) descriptor -> 't -> 'res inline -> 'res

val compare : 'a schema -> 'a -> 'a -> int

include module type of Schema_intf

val required : (_, _) requirement -> bool

val map : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t

val pp : Format.formatter -> _ t -> unit

val pp_descriptor : Format.formatter -> _ descriptor -> unit

val extract_field_schema : 'a field_schema -> bool * 'a t

val descriptor_fields : 'a descriptor -> string list

type 'res inline = { f : 't. 't field -> 'res }

val inline : 't descriptor -> 'res inline -> 'res

type 'res inline_v = { f : 't. 't field -> 't -> 'res }

val inline_v : 't descriptor -> 't -> 'res inline_v -> 'res

val compare : 'a t -> 'a -> 'a -> int

type pack

val pack : _ t -> pack

type 'res unpacker = { unpack : 't. 't t -> 'res }

val unpack : 'res unpacker -> pack -> 'res

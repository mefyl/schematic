include module type of Schema_intf

val bool_schema : bool schema

val int_schema : int schema

val list_schema : 'a schema -> 'a list schema

val map : ('a -> 'b) -> ('b -> 'a) -> 'a schema -> 'b schema

val mapping_schema : 'a schema -> (string * 'a) list schema

val string_schema : string schema

val string_const_schema : string -> unit schema

val unit_schema : unit schema

val pp : Format.formatter -> _ schema -> unit

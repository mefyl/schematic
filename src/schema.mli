include module type of Schema_intf

val int_schema : int schema

val string_schema : string schema

val bool_schema : bool schema

val list_schema : 'a schema -> 'a list schema

val mapping_schema : 'a schema -> (string * 'a) list schema

val map : ('a -> 'b) -> ('b -> 'a) -> 'a schema -> 'b schema

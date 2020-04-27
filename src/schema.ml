include Schema_intf

let unit_schema = Null

let int_schema = Int

let string_schema = String

let string_const_schema s = String_const s

let bool_schema = Bool

let list_schema schema = List schema

let mapping_schema schema = Mapping schema

let map cons decons schema = Map (cons, decons, schema)

include Schema_intf

let unit_schema = Null

let int_schema = Int

let string_schema = String

let bool_schema = Bool

let list_schema schema = List schema

let mapping_schema schema = Mapping schema

let map cons decons schema = Map (cons, decons, schema)

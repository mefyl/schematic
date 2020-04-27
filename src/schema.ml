include Schema_intf

let unit_schema = { id = None; descriptor = Null }

let int_schema = { id = None; descriptor = Int }

let string_schema = { id = None; descriptor = String }

let string_const_schema s = { id = None; descriptor = String_const s }

let bool_schema = { id = None; descriptor = Bool }

let list_schema schema = { id = None; descriptor = List schema }

let mapping_schema schema = { id = None; descriptor = Mapping schema }

let map cons decons { descriptor; _ } =
  { id = None; descriptor = Map (cons, decons, descriptor) }

module Schemas = struct
  let unit_schema = { Schema.id = None; descriptor = Null }

  let int_schema = { Schema.id = None; descriptor = Int }

  let string_schema = { Schema.id = None; descriptor = String }

  let string_const_schema s = { Schema.id = None; descriptor = String_const s }

  let bool_schema = { Schema.id = None; descriptor = Bool }

  let list_schema schema = { Schema.id = None; descriptor = List schema }

  let mapping_schema schema = { Schema.id = None; descriptor = Mapping schema }
end

module Ptime = struct
  include Ptime

  let schema = { Schema.id = None; descriptor = Time }

  type local = Ptime.t * Ptime.tz_offset_s

  let local_schema = { Schema.id = None; descriptor = Time_local }

  let date_schema = { Schema.id = None; descriptor = Date }
end

include Schemas

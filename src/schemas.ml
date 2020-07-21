module Ptime = struct
  include Ptime

  let schema = { Schema.id = None; descriptor = Time }

  type local = Ptime.t * Ptime.tz_offset_s

  let local_schema = { Schema.id = None; descriptor = Time_local }

  type date_alias = int * int * int [@@deriving schema]

  let date_schema = date_alias_schema
end

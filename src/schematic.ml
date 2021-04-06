module Json = struct
  include Json

  module Schema = struct
    type t = Json_schema.json_schema

    let of_schema = Json_schema.of_schema

    let schema = Json_schema.json_schema_schema
  end
end

module Schema = struct
  include Schema
  module Schemas = Schemas
end

module Coding = Coding
module Typescript = Typescript

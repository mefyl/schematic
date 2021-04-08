module Error = Error

module Json = struct
  include Json
  module Schema = Json_schema
end

module Schema = struct
  include Schema
  module Schemas = Schemas
end

module Coding = Coding
module Typescript = Typescript

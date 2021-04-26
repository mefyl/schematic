module Error = Error

module Json = struct
  include Json
  module Schema = Json_schema
end

module Schema = struct
  include Schema
  module Schemas = Schemas
end

type 'a schema = 'a Schema.t

module Coding = Coding
module Typescript = Typescript

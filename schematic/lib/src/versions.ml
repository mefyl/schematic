module V0_12_0 = struct
  module Error = Error

  module Json = struct
    include Json
    module Schema = Json_schema
  end

  module Schema = Schema
  module Schemas = Schemas

  type 'a schema = 'a Schema.t

  module Coding = Coding
  module Typescript = Typescript
  module Version = Version
end

module V0_12_1 = struct
  include V0_12_0
  module Base64 = B64
end

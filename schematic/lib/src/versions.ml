module V0_14_0 = struct
  module Error = Error
  module Base64 = B64

  module Json = struct
    include Json
    module Schema = Json_schema
  end

  module Schema = Schema
  module Schemas = Schemas

  type 'a schema = 'a Schema.t

  module Coding = Coding
  module Pp = Pp
  module Typescript = Typescript
  module Version = Version
end

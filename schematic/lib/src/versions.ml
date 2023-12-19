module V0_19_0 = struct
  module Error = Error
  module Base64 = B64

  module Json = struct
    include Json
    module Schema = Json_schema
  end

  module Marks = Marks
  module Schema = Schema
  module Schemas = Schemas
  module Coding = Coding
  module Pp = Pp
  module Typescript = Typescript
  module Version = Version
end

module V0_19_1 = V0_19_0

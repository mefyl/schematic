module V0_18_0 = struct
  module Error = Error
  module Base64 = B64

  module Json = struct
    include Json
    module Schema = Json_schema
  end

  module Schema = Schema
  module Schemas = Schemas
  module Coding = Coding
  module Pp = Pp
  module Typescript = Typescript
  module Version = Version
end

module V0_18_2 = V0_18_0
module V0_18_4 = V0_18_2
module V0_18_5 = V0_18_4
module V0_18_6 = V0_18_5

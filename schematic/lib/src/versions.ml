module V0_17_4 = struct
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

module V0_17_3 = V0_17_4
module V0_17_2 = V0_17_3
module V0_17_1 = V0_17_2
module V0_17_0 = V0_17_1

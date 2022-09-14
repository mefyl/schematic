module V0_14_8 = struct
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

module V0_14_7 = V0_14_8
module V0_14_6 = V0_14_7
module V0_14_5 = V0_14_6
module V0_14_4 = V0_14_5
module V0_14_3 = V0_14_4
module V0_14_2 = V0_14_3
module V0_14_1 = V0_14_2
module V0_14_0 = V0_14_1

module V0_14_7 = struct
  include V0_14_6
  module C = C
end

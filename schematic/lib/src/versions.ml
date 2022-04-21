module V0_13_2 = struct
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
  module Typescript = Typescript
  module Version = Version
end

module V0_13_1 = V0_13_2

module V0_13_0 = struct
  include V0_13_1

  module Json = struct
    include Json
    module String : Coding.S with type t = string = String
  end
end

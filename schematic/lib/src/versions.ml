module V0_19_0 = struct
  module Error = Error
  module Base64 = B64

  module Json = struct
    include Json
    module Schema = Json_schema
  end

  module Kotlin = Kotlin
  module Marks = Marks
  module Schema = Schema
  module Schemas = Schemas
  module Coding = Coding
  module Pp = Pp
  module Typescript = Typescript
  module Version = Version
end

module V0_19_1 = V0_19_0
module V0_19_2 = V0_19_1
module V0_19_3 = V0_19_2
module V0_19_4 = V0_19_3
module V0_19_5 = V0_19_4
module V0_19_6 = V0_19_5

module V0_19_7 = struct
  include V0_19_6
  module Swift = Swift
end

module V0_19_8 = struct
  include V0_19_7
  module Marshal = Marshal
  module Streaming = Streaming
end

module V0_22_0 = struct
  module Error = Error
  module Base64 = B64
  module Coding = Coding

  module Json = struct
    include Json
    module Schema = Json_schema
  end

  module Kotlin = Kotlin
  module Marks = Marks
  module Marshal = Marshal
  module Pp = Pp
  module Schema = Schema
  module Schemas = Schemas
  module Streaming = Streaming
  module Swift = Swift
  module Typescript = Typescript
  module Version = Version
end

include Intf
include Utils
module Attributes = Attributes
module Expand = Expand
module Plugin = Plugin

let schema_of_type ~context ~type_context ~loc t =
  Schema.(schema_of_expanded ~loc @@ encode_type ~context ~type_context t)

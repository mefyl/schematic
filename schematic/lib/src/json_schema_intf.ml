type uri_reference = string

and uri = string

and type_ =
  | Array
  | Boolean
  | Integer
  | Null
  | Number
  | Object
  | String
  | OneOf of type_ list
[@@deriving eq, schema]

type core = {
  id : uri_reference option; [@schematic.as "$id"]
  schema : uri option; [@schematic.as "$schema"]
  anchor : string option; [@schematic.as "$anchor"]
  ref : uri_reference option; [@schematic.as "$ref"]
  recursive_ref : uri_reference option; [@schematic.as "$recursiveRef"]
  recursive_anchor : bool option; [@schematic.as "$recursiveAnchor"]
  vocabulary : (uri * bool) list option; [@schematic.as "$vocabulary"]
  comment : string option; [@schematic.as "$comment"]
  defs : (string * t) list option;
      [@schematic.as "$defs"] [@schematic.default []]
}

and additional_properties =
  | Properties_schema of t
  | Allowed of bool

and items =
  | Items_schema of t
  | Tuple_schema of t list

and applicator = {
  additionalItems : t option;
  unevaluatedItems : t option;
  items : items option;
  contains : t option;
  additionalProperties : additional_properties option;
  unevaluatedProperties : t option;
  properties : (string * t) list option;
  patternProperties : (string * t) list option;
  dependentSchema : (string * t) list option;
  propertyNames : t option;
  if_ : t option;
  then_ : t option;
  else_ : t option;
  not : t option;
  allOf : t list option;
  anyOf : t list option;
  oneOf : t list option;
}

and validation = {
  multipleOf : int option; [@schematic.properties { minimum = 1 }]
  maximum : int option;
  exclusiveMaximum : int option;
  minimum : int option;
  exclusiveMinimum : int option;
  maxLength : int option; [@schematic.properties { minimum = 0 }]
  minLength : int option;
      [@schematic.default 0] [@schematic.properties { minimum = 0 }]
  pattern : string option;
  maxItems : int option; [@schematic.properties { minimum = 0 }]
  minItems : int option; [@default 0] [@schematic.properties { minimum = 0 }]
  uniqueItems : bool option; [@schematic.default false]
  maxContains : int option; [@schematic.properties { minimum = 0 }]
  minContains : int option; [@default 0] [@schematic.properties { minimum = 1 }]
  maxProperties : int option; [@schematic.properties { minimum = 0 }]
  minProperties : int option;
      [@schematic.properties { minimum = 0 }] [@default 0]
  required : string list option;
      [@schematic.properties { unique = true }] [@default []]
  dependentRequired : (string * string list) list option;
  const : Schemas.any option;
  enum : Schemas.any list option;
  type_ : (type_, type_ list) Schemas.Either.t option;
}

and metadata = {
  title : string option;
  description : string option;
  default : Schemas.any option;
  deprecated : bool; [@schematic.default false]
  readOnly : bool; [@schematic.default false]
  writeOnly : bool; [@schematic.default false]
  examples : Schemas.any list; [@schematic.default []]
}

and t = {
  applicator : applicator; [@schematic.inline]
  core : core; [@schematic.inline]
  metadata : metadata; [@schematic.inline]
  validation : validation; [@schematic.inline]
}
[@@deriving eq, schema]

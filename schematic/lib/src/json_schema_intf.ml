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

and 't additional_properties =
  | Properties_schema of 't
  | Allowed of bool

and 't items =
  | Items_schema of 't
  | Tuple_schema of 't list
[@@deriving eq, schema]

type core = {
  id : uri_reference option; [@schematic.as "$id"]
  schema : uri option; [@schematic.as "$schema"]
  anchor : string option; [@schematic.as "$anchor"]
  ref : uri_reference option; [@schematic.as "$ref"]
  recursive_ref : uri_reference option; [@schematic.as "$recursiveRef"]
  recursive_anchor : bool; [@schematic.as "$recursiveAnchor"] [@default false]
  vocabulary : (uri * bool) list; [@schematic.as "$vocabulary"] [@default []]
  comment : string option; [@schematic.as "$comment"]
  defs : (string * t) list; [@schematic.as "$defs"] [@default []]
}

and applicator = {
  additionalItems : t option;
  unevaluatedItems : t option;
  items : t items option;
  contains : t option;
  additionalProperties : t additional_properties; [@default Allowed true]
  unevaluatedProperties : t option;
  properties : (string * t) list; [@default []]
  patternProperties : (string * t) list; [@default []]
  dependentSchema : (string * t) list; [@default []]
  propertyNames : t option;
  if_ : t option;
  then_ : t option;
  else_ : t option;
  not : t option;
  all_of : t list; [@default []] [@schematic.as "allOf"]
  any_of : t list; [@default []] [@schematic.as "anyOf"]
  one_of : t list; [@default []] [@schematic.as "oneOf"]
}

and validation = {
  multipleOf : int option; [@schematic.properties { minimum = 1 }]
  maximum : int option;
  exclusiveMaximum : int option;
  minimum : int option;
  exclusiveMinimum : int option;
  maxLength : int option; [@schematic.properties { minimum = 0 }]
  minLength : int option; [@schematic.properties { minimum = 0 }]
  pattern : string option;
  maxItems : int option; [@schematic.properties { minimum = 0 }]
  minItems : int option; [@schematic.properties { minimum = 0 }]
  uniqueItems : bool; [@default false]
  maxContains : int option; [@schematic.properties { minimum = 0 }]
  minContains : int option; [@schematic.properties { minimum = 1 }]
  maxProperties : int option; [@schematic.properties { minimum = 0 }]
  minProperties : int option; [@schematic.properties { minimum = 0 }]
  required : string list;
      [@schematic.properties { unique = true }] [@default []]
  dependentRequired : (string * string list) list; [@default []]
  const : Schemas.any option;
  enum : Schemas.any list; [@default []]
  type_ : (type_, type_ list) Schemas.Either.t option;
}

and metadata = {
  title : string option;
  description : string option;
  default : Schemas.any option;
  deprecated : bool; [@default false]
  readOnly : bool; [@default false]
  writeOnly : bool; [@default false]
  examples : Schemas.any list; [@default []]
}

and t = {
  applicator : applicator; [@schematic.inline]
  core : core; [@schematic.inline]
  metadata : metadata; [@schematic.inline]
  validation : validation; [@schematic.inline]
}
[@@deriving eq, make, schema]

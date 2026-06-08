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
[@@deriving eq, schema]

type core = {
  id : uri_reference option; [@schematic.as "$id"]
  schema : uri option; [@schematic.as "$schema"]
  anchor : string option; [@schematic.as "$anchor"]
  ref : uri_reference option; [@schematic.as "$ref"]
  dynamic_ref : uri_reference option; [@schematic.as "$dynamicRef"]
  dynamic_anchor : string option; [@schematic.as "$dynamicAnchor"]
  vocabulary : (uri * bool) list; [@schematic.as "$vocabulary"] [@default []]
  comment : string option; [@schematic.as "$comment"]
  defs : (string * t) list; [@schematic.as "$defs"] [@default []]
}

and applicator = {
  unevaluatedItems : t option;
  prefixItems : t list; [@default []]
  items : t option;
  contains : t option;
  additionalProperties : t additional_properties; [@default Allowed true]
  unevaluatedProperties : t option;
  properties : (string * t) list; [@default []]
  patternProperties : (string * t) list; [@default []]
  dependentSchemas : (string * t) list; [@default []]
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
  const : Schemas.json option;
  enum : Schemas.json list; [@default []]
  type_ : type_ list;
      [@default []] [@schematic.schema one_or_many_versioned type_schema]
}

and metadata = {
  title : string option;
  description : string option;
  default : Schemas.json option;
  deprecated : bool; [@default false]
  readOnly : bool; [@default false]
  writeOnly : bool; [@default false]
  examples : Schemas.json list; [@default []]
}

and t = {
  applicator : applicator; [@schematic.inline]
  core : core; [@schematic.inline]
  metadata : metadata; [@schematic.inline]
  validation : validation; [@schematic.inline]
}
[@@deriving eq, make, schema]

open Batteries

type ('a, 'b) either =
  | This of 'a
  | That of 'b
[@@deriving schema]

type uri_reference = string

and uri = string

and any = unit

and core =
  { id : uri_reference option [@schematic.as "$id"]
  ; schema : uri option [@schematic.as "$schema"]
  ; anchor : string option [@schematic.as "$anchor"]
  ; ref : uri_reference option [@schematic.as "$ref"]
  ; recursive_ref : uri_reference option [@schematic.as "$recursiveRef"]
  ; recursive_anchor : bool option [@schematic.as "$recursiveAnchor"]
  ; vocabulary : (uri * bool) list option [@schematic.as "$vocabulary"]
  ; comment : string option [@schematic.as "$comment"]
  ; defs : (string * json_schema) list option
        [@schematic.as "$defs"] [@schematic.properties { default = [] }]
  }

and type_ =
  | Array
  | Boolean
  | Integer
  | Null
  | Number
  | Object
  | String
  | OneOf of type_ list

and additional_properties =
  | Properties_schema of json_schema
  | Allowed of bool

and items =
  | Items_schema of json_schema
  | Tuple_schema of json_schema list

and applicator =
  { additionalItems : json_schema option
  ; unevaluatedItems : json_schema option
  ; items : items option
  ; contains : json_schema option
  ; additionalProperties : additional_properties option
  ; unevaluatedProperties : json_schema option
  ; properties : (string * json_schema) list option
  ; patternProperties : (string * json_schema) list option
  ; dependentSchema : (string * json_schema) list option
  ; propertyNames : json_schema option
  ; if_ : json_schema option
  ; then_ : json_schema option
  ; else_ : json_schema option
  ; not : json_schema option
  ; allOf : json_schema list option
  ; anyOf : json_schema list option
  ; oneOf : json_schema list option
  }

and validation =
  { multipleOf : int option [@schematic.properties { minimum = 1 }]
  ; maximum : int option
  ; exclusiveMaximum : int option
  ; minimum : int option
  ; exclusiveMinimum : int option
  ; maxLength : int option [@schematic.properties { minimum = 0 }]
  ; minLength : int option [@schematic.properties { default = 0; minimum = 0 }]
  ; pattern : string option
  ; maxItems : int option [@schematic.properties { minimum = 0 }]
  ; minItems : int option [@schematic.properties { default = 0; minimum = 0 }]
  ; uniqueItems : bool option [@schematic.properties { default = false }]
  ; maxContains : int option [@schematic.properties { minimum = 0 }]
  ; minContains : int option
        [@schematic.properties { default = 0; minimum = 1 }]
  ; maxProperties : int option [@schematic.properties { minimum = 0 }]
  ; minProperties : int option
        [@schematic.properties { default = 0; minimum = 0 }]
  ; required : string list option
        [@schematic.properties { default = []; unique = true }]
  ; dependentRequired : (string * string list) list option
  ; const : any option
  ; enum : any list option
  ; type_ : type_ option
  }

and json_schema =
  { core : core [@schematic.inline]
  ; applicator : applicator [@schematic.inline]
  ; validation : validation [@schematic.inline]
  }
[@@deriving schema]

let core_empty =
  { id = None
  ; schema = None
  ; anchor = None
  ; ref = None
  ; recursive_ref = None
  ; recursive_anchor = None
  ; vocabulary = None
  ; comment = None
  ; defs = None
  }

let applicator_empty =
  { additionalItems = None
  ; unevaluatedItems = None
  ; items = None
  ; contains = None
  ; additionalProperties = None
  ; unevaluatedProperties = None
  ; properties = None
  ; patternProperties = None
  ; dependentSchema = None
  ; propertyNames = None
  ; if_ = None
  ; then_ = None
  ; else_ = None
  ; not = None
  ; allOf = None
  ; anyOf = None
  ; oneOf = None
  }

let validation_empty =
  { multipleOf = None
  ; maximum = None
  ; exclusiveMaximum = None
  ; minimum = None
  ; exclusiveMinimum = None
  ; maxLength = None
  ; minLength = None
  ; pattern = None
  ; maxItems = None
  ; minItems = None
  ; uniqueItems = None
  ; maxContains = None
  ; minContains = None
  ; maxProperties = None
  ; minProperties = None
  ; required = None
  ; dependentRequired = None
  ; const = None
  ; enum = None
  ; type_ = None
  }

let json_schema_empty =
  { core = core_empty
  ; validation = validation_empty
  ; applicator = applicator_empty
  }

let this = function
  | This v -> v
  | That _ -> failwith "that instead of this"

let that = function
  | This _ -> failwith "this instead of that"
  | That v -> v

module Ids = Set.Make (String)

let of_schema schema =
  let id_name s = "#" ^ s in
  let rec of_schema : type t. Ids.t -> t Schema.schema -> Ids.t * json_schema =
   fun marks -> function
    | { id = None; descriptor } -> of_descriptor marks descriptor
    | { id = Some id; descriptor = _ } when Ids.mem id marks ->
      ( marks
      , { json_schema_empty with
          core = { core_empty with ref = Some (id_name id) }
        } )
    | { id = Some id; descriptor } ->
      let marks = Ids.add id marks in
      let marks, schema = of_descriptor marks descriptor in
      (marks, { schema with core = { schema.core with id = Some (id_name id) } })
  and of_descriptor :
      type t. Ids.t -> t Schema.descriptor -> Ids.t * json_schema =
   fun marks schema ->
    let rec validation : type t. t Schema.descriptor -> validation = function
      | Schema.Bool -> { validation_empty with type_ = Some Boolean }
      | Schema.End -> failwith "unexpected End in schema"
      | Schema.Int -> { validation_empty with type_ = Some Integer }
      | Schema.List _ -> { validation_empty with type_ = Some Array }
      | Schema.Map (_, _, schema) -> validation schema
      | Schema.Mapping _ -> validation_empty
      | Schema.Null -> { validation_empty with type_ = Some Null }
      | Schema.String -> { validation_empty with type_ = Some String }
      | Schema.String_const _ ->
        { validation_empty with
          type_ = Some String
        ; const = Some () (* FIXME *)
        }
      | Schema.Object _ -> { validation_empty with type_ = Some Object }
      | Schema.Tuple _ -> { validation_empty with type_ = Some Array }
      | Schema.Union _ -> validation_empty
    and applicator :
        type t. Ids.t -> t Schema.descriptor -> Ids.t * applicator option =
     fun marks -> function
      | Schema.Bool -> (marks, Some applicator_empty)
      | Schema.End -> (marks, None)
      | Schema.Int -> (marks, Some applicator_empty)
      | Schema.List schema ->
        let marks, schema = of_schema marks schema in
        ( marks
        , Some { applicator_empty with items = Some (Items_schema schema) } )
      | Schema.Map (_, _, schema) -> applicator marks schema
      | Schema.Mapping schema ->
        let marks, schema = of_schema marks schema in
        ( marks
        , Some
            { applicator_empty with
              additionalProperties = Some (Properties_schema schema)
            } )
      | Schema.Null -> (marks, Some applicator_empty)
      | Schema.String -> (marks, Some applicator_empty)
      | Schema.String_const _ -> (marks, Some applicator_empty)
      | Schema.Object
          { field
          ; title = _
          ; description = _
          ; requirement = _
          ; schema
          ; inline = _
          ; rest
          } -> (
        let marks, schema = of_schema marks schema in
        let field = (field, schema) in
        match applicator marks rest with
        | marks, None ->
          ( marks
          , Some
              { applicator_empty with
                properties = Some [ field ]
              ; additionalProperties = Some (Allowed false)
              } )
        | marks, Some applicator ->
          ( marks
          , Some
              { applicator with
                properties =
                  Some
                    ( field
                    :: Option.get_exn applicator.properties (Failure "no 1") )
              } ) )
      | Schema.Tuple { schema; rest } -> (
        let marks, field = of_schema marks schema in
        match applicator marks rest with
        | marks, None ->
          ( marks
          , Some { applicator_empty with items = Some (Tuple_schema [ field ]) }
          )
        | marks, Some applicator ->
          ( marks
          , Some
              { applicator with
                items =
                  Some
                    (Tuple_schema
                       ( field
                       ::
                       ( match
                           Option.get_exn applicator.items (Failure "no 2")
                         with
                       | Tuple_schema l -> l
                       | Items_schema _ -> failwith "mixed list and tuple" ) ))
              } ) )
      | Schema.Union cases ->
        let f (Schema.Case { schema; _ }) (marks, l) =
          let marks, schema = of_schema marks schema in
          (marks, schema :: l)
        in
        let marks, schemas = List.fold_right f cases (marks, []) in
        (marks, Some { applicator_empty with oneOf = Some schemas })
    in
    ( marks
    , { core =
          { core_empty with
            schema = Some "https://json-schema.org/draft/2019-09/schema"
          }
      ; applicator =
          Option.get_exn (snd @@ applicator marks schema) (Failure "no 3")
      ; validation = validation schema
      } )
  in
  of_schema Ids.empty schema |> snd

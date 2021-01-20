open Base
open Schemas

let ( % ) = Fn.compose

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
[@@deriving schema]

type core = {
  id : uri_reference option; [@schematic.as "$id"]
  schema : uri option; [@schematic.as "$schema"]
  anchor : string option; [@schematic.as "$anchor"]
  ref : uri_reference option; [@schematic.as "$ref"]
  recursive_ref : uri_reference option; [@schematic.as "$recursiveRef"]
  recursive_anchor : bool option; [@schematic.as "$recursiveAnchor"]
  vocabulary : (uri * bool) list option; [@schematic.as "$vocabulary"]
  comment : string option; [@schematic.as "$comment"]
  defs : (string * json_schema) list option;
      [@schematic.as "$defs"] [@schematic.properties { default = [] }]
}

and additional_properties =
  | Properties_schema of json_schema
  | Allowed of bool

and items =
  | Items_schema of json_schema
  | Tuple_schema of json_schema list

and applicator = {
  additionalItems : json_schema option;
  unevaluatedItems : json_schema option;
  items : items option;
  contains : json_schema option;
  additionalProperties : additional_properties option;
  unevaluatedProperties : json_schema option;
  properties : (string * json_schema) list option;
  patternProperties : (string * json_schema) list option;
  dependentSchema : (string * json_schema) list option;
  propertyNames : json_schema option;
  if_ : json_schema option;
  then_ : json_schema option;
  else_ : json_schema option;
  not : json_schema option;
  allOf : json_schema list option;
  anyOf : json_schema list option;
  oneOf : json_schema list option;
}

and validation = {
  multipleOf : int option; [@schematic.properties { minimum = 1 }]
  maximum : int option;
  exclusiveMaximum : int option;
  minimum : int option;
  exclusiveMinimum : int option;
  maxLength : int option; [@schematic.properties { minimum = 0 }]
  minLength : int option; [@schematic.properties { default = 0; minimum = 0 }]
  pattern : string option;
  maxItems : int option; [@schematic.properties { minimum = 0 }]
  minItems : int option; [@schematic.properties { default = 0; minimum = 0 }]
  uniqueItems : bool option; [@schematic.properties { default = false }]
  maxContains : int option; [@schematic.properties { minimum = 0 }]
  minContains : int option; [@schematic.properties { default = 0; minimum = 1 }]
  maxProperties : int option; [@schematic.properties { minimum = 0 }]
  minProperties : int option;
      [@schematic.properties { default = 0; minimum = 0 }]
  required : string list option;
      [@schematic.properties { default = []; unique = true }]
  dependentRequired : (string * string list) list option;
  const : any option;
  enum : any list option;
  type_ : (type_, type_ list) Either.t option;
}

and metadata = {
  title : string option;
  description : string option;
  default : any option;
  deprecated : bool; [@schematic.properties { default = false }]
  readOnly : bool; [@schematic.properties { default = false }]
  writeOnly : bool; [@schematic.properties { default = false }]
  examples : any list; [@schematic.properties { default = [] }]
}

and json_schema = {
  applicator : applicator; [@schematic.inline]
  core : core; [@schematic.inline]
  metadata : metadata; [@schematic.inline]
  validation : validation; [@schematic.inline]
}
[@@deriving schema]

let applicator_empty =
  {
    additionalItems = None;
    unevaluatedItems = None;
    items = None;
    contains = None;
    additionalProperties = None;
    unevaluatedProperties = None;
    properties = None;
    patternProperties = None;
    dependentSchema = None;
    propertyNames = None;
    if_ = None;
    then_ = None;
    else_ = None;
    not = None;
    allOf = None;
    anyOf = None;
    oneOf = None;
  }

let core_empty =
  {
    id = None;
    schema = None;
    anchor = None;
    ref = None;
    recursive_ref = None;
    recursive_anchor = None;
    vocabulary = None;
    comment = None;
    defs = None;
  }

and metadata_empty =
  {
    title = None;
    description = None;
    default = None;
    deprecated = false;
    readOnly = false;
    writeOnly = false;
    examples = [];
  }

let validation_empty =
  {
    multipleOf = None;
    maximum = None;
    exclusiveMaximum = None;
    minimum = None;
    exclusiveMinimum = None;
    maxLength = None;
    minLength = None;
    pattern = None;
    maxItems = None;
    minItems = None;
    uniqueItems = None;
    maxContains = None;
    minContains = None;
    maxProperties = None;
    minProperties = None;
    required = None;
    dependentRequired = None;
    const = None;
    enum = None;
    type_ = None;
  }

let json_schema_empty =
  {
    applicator = applicator_empty;
    core = core_empty;
    metadata = metadata_empty;
    validation = validation_empty;
  }

type ids = (string, String.comparator_witness) Set.t

type date_tuple = int * int * int [@@deriving schema]

let of_schema schema =
  let id_name s = "#" ^ s in
  let rec of_schema : type t. ids -> t Schema.schema -> ids * json_schema =
   fun marks -> function
    | { id = None; descriptor } -> of_descriptor marks descriptor
    | { id = Some id; descriptor = _ } when Set.mem marks id ->
      ( marks,
        {
          json_schema_empty with
          core = { core_empty with ref = Some (id_name id) };
        } )
    | { id = Some id; descriptor } ->
      let marks = Set.add marks id in
      let marks, schema = of_descriptor marks descriptor in
      (marks, { schema with core = { schema.core with id = Some (id_name id) } })
  and of_descriptor :
      type t. ids -> (t, _) Schema.descriptor -> ids * json_schema =
   fun marks schema ->
    let rec validation : type t. (t, _) Schema.descriptor -> validation =
      function
      | Schema.Any -> validation_empty
      | Schema.Bool ->
        { validation_empty with type_ = Some (Either.first Boolean) }
      | Schema.Date -> validation date_tuple_schema.descriptor
      | Schema.Float ->
        { validation_empty with type_ = Some (Either.first Number) }
      | Schema.Int
      | Schema.Int64 ->
        { validation_empty with type_ = Some (Either.first Integer) }
      | Schema.List _ ->
        { validation_empty with type_ = Some (Either.first Array) }
      | Schema.Map (_, _, schema) -> validation schema
      | Schema.Mapping _ -> validation_empty
      | Schema.Null ->
        { validation_empty with type_ = Some (Either.first Null) }
      | Schema.Nullable s -> (
        let next = validation s.descriptor in
        match next.type_ with
        | None -> { next with type_ = Some (Either.first Null) }
        | Some (Either.First t) ->
          { next with type_ = Some (Either.second [ t; Null ]) }
        | Some (Either.Second types) ->
          {
            next with
            type_ =
              Some
                (Either.second
                   (List.dedup_and_sort ~compare:Poly.compare (Null :: types)));
          } )
      | Schema.String ->
        { validation_empty with type_ = Some (Either.first String) }
      | Schema.String_const v ->
        {
          validation_empty with
          type_ = Some (Either.first String);
          const = Some (`String v);
        }
      | Schema.Time ->
        { validation_empty with type_ = Some (Either.first String) }
      | Schema.Time_local ->
        { validation_empty with type_ = Some (Either.first String) }
      | Schema.Object _ ->
        { validation_empty with type_ = Some (Either.first Object) }
      | Schema.Tuple _ ->
        { validation_empty with type_ = Some (Either.first Array) }
      | Schema.Union _ -> validation_empty
    and applicator :
        type t kind.
        ids -> (t, kind) Schema.descriptor -> ids * applicator option =
     fun marks -> function
      | Schema.Any -> (marks, Some applicator_empty)
      | Schema.Bool -> (marks, Some applicator_empty)
      | Schema.Date -> applicator marks date_tuple_schema.descriptor
      | Schema.Float
      | Schema.Int
      | Schema.Int64 ->
        (marks, Some applicator_empty)
      | Schema.List schema ->
        let marks, schema = of_schema marks schema in
        ( marks,
          Some { applicator_empty with items = Some (Items_schema schema) } )
      | Schema.Map (_, _, schema) -> applicator marks schema
      | Schema.Mapping schema ->
        let marks, schema = of_schema marks schema in
        ( marks,
          Some
            {
              applicator_empty with
              additionalProperties = Some (Properties_schema schema);
            } )
      | Schema.Null -> (marks, Some applicator_empty)
      | Schema.Nullable s -> applicator marks s.descriptor
      | Schema.String -> (marks, Some applicator_empty)
      | Schema.String_const _ -> (marks, Some applicator_empty)
      | Schema.Time -> (marks, Some applicator_empty)
      | Schema.Time_local -> (marks, Some applicator_empty)
      | Schema.Object _ as o ->
        let rec get_fields :
            type t kind.
            ids ->
            (t, kind) Schema.descriptor ->
            ids * (string * json_schema) list option =
         fun marks -> function
          | Schema.Null -> (marks, None)
          | Schema.Nullable s -> get_fields marks s.descriptor
          | Schema.Map (_, _, d) -> get_fields marks d
          | Schema.Object
              {
                description;
                examples;
                field;
                maximum;
                minimum;
                requirement;
                rest;
                schema;
                title;
              } -> (
            let inline, schema = Schema.extract_field_schema schema in
            let marks, fields =
              if inline then
                let marks, fields = get_fields marks schema.descriptor in
                match fields with
                | Some fields -> (marks, fields)
                | None ->
                  let marks, schema = of_schema marks schema in
                  (marks, [ (field, schema) ])
              else
                let marks, json_schema = of_schema marks schema
                and to_json v = Json.to_ezjsonm @@ Json.encode schema v in
                ( marks,
                  [
                    ( field,
                      {
                        json_schema with
                        metadata =
                          {
                            json_schema.metadata with
                            default =
                              ( match requirement with
                              | Schema.Default v -> Some (to_json v)
                              | _ -> None );
                            description;
                            title =
                              Option.return
                                (Option.value ~default:(String.capitalize field)
                                   title);
                            examples = List.map ~f:to_json examples;
                          };
                        validation =
                          { json_schema.validation with maximum; minimum };
                      } );
                  ] )
            in
            match applicator marks rest with
            | marks, None -> (marks, Some fields)
            | marks, Some applicator ->
              ( marks,
                Some (fields @ Option.value ~default:[] applicator.properties)
              ) )
          | Schema.Any
          | Schema.Bool ->
            (marks, None)
          | Schema.Date -> (marks, None)
          | Schema.Float -> (marks, None)
          | Schema.Int
          | Schema.Int64 ->
            (marks, None)
          | Schema.List _ -> (marks, None)
          | Schema.Mapping _ -> (marks, None)
          | Schema.String -> (marks, None)
          | Schema.Time -> (marks, None)
          | Schema.Time_local -> (marks, None)
          | Schema.String_const _ -> (marks, None)
          | Schema.Tuple _ -> (marks, None)
          | Schema.Union _ -> (marks, None)
        in
        let marks, fields = get_fields marks o in
        ( marks,
          Some
            {
              applicator_empty with
              properties = fields;
              additionalProperties = Some (Allowed false);
            } )
      | Schema.Tuple { schema; rest } -> (
        let marks, field = of_schema marks schema in
        match applicator marks rest with
        | marks, None ->
          ( marks,
            Some { applicator_empty with items = Some (Tuple_schema [ field ]) }
          )
        | marks, Some applicator ->
          ( marks,
            Some
              {
                applicator with
                items =
                  Some
                    (Tuple_schema
                       ( field
                       ::
                       ( match applicator.items with
                       | None -> []
                       | Some (Tuple_schema l) -> l
                       | Some (Items_schema _) ->
                         failwith "mixed list and tuple" ) ));
              } ) )
      | Schema.Union cases ->
        let f (Schema.Case { schema; _ }) (marks, l) =
          let marks, schema = of_schema marks schema in
          (marks, schema :: l)
        in
        let marks, schemas = List.fold_right ~f ~init:(marks, []) cases in
        (marks, Some { applicator_empty with oneOf = Some schemas })
    in
    ( marks,
      {
        applicator = Option.value_exn (snd @@ applicator marks schema);
        core =
          {
            core_empty with
            schema = Some "https://json-schema.org/draft/2019-09/schema";
          };
        metadata = metadata_empty;
        validation = validation schema;
      } )
  in
  of_schema (Set.empty (module String)) schema |> snd

open Base
include Json_schema_intf

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

let empty =
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
  let rec of_schema : type u. ids -> u Schema.t -> ids * t =
   fun marks -> function
    | { id = None; descriptor; _ } -> of_descriptor marks descriptor
    | { id = Some id; _ } when Set.mem marks id ->
      (marks, { empty with core = { core_empty with ref = Some (id_name id) } })
    | { id = Some id; descriptor; _ } ->
      let marks = Set.add marks id in
      let marks, schema = of_descriptor marks descriptor in
      (marks, { schema with core = { schema.core with id = Some (id_name id) } })
  and of_descriptor : type u. ids -> u Schema.descriptor -> ids * t =
   fun marks schema ->
    let rec validation : type t. t Schema.descriptor -> validation = function
      | Schema.Any -> validation_empty
      | Schema.Bool ->
        { validation_empty with type_ = Some (Either.first Boolean) }
      | Schema.Date -> validation date_tuple_schema.descriptor
      | Schema.Float ->
        { validation_empty with type_ = Some (Either.first Number) }
      | Schema.Ignore -> validation_empty
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
          })
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
        type u. ids -> u Schema.descriptor -> ids * applicator option =
     fun marks -> function
      | Schema.Any -> (marks, Some applicator_empty)
      | Schema.Bool -> (marks, Some applicator_empty)
      | Schema.Date -> applicator marks date_tuple_schema.descriptor
      | Schema.Float
      | Schema.Ignore ->
        (marks, Some applicator_empty)
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
      | Schema.Object o ->
        let rec get_fields :
            type u. ids -> u Schema.field -> ids * (string * t) list option =
         fun marks -> function
          | Schema.FieldEnd -> (marks, None)
          | Schema.Field
              {
                description;
                examples;
                field;
                maximum;
                minimum;
                omit = (* FIXME *) _;
                requirement;
                rest;
                schema;
                title;
              } -> (
            let marks, fields =
              match schema with
              | Inline schema
              | Inline_try schema -> (
                match applicator marks schema.descriptor with
                | marks, Some applicator ->
                  (marks, Option.value ~default:[] applicator.properties)
                | marks, None -> (marks, []))
              | Outline schema ->
                let marks, t = of_schema marks schema
                and to_json v = Json.to_ezjsonm @@ Json.encode schema v in
                ( marks,
                  [
                    ( field,
                      {
                        t with
                        metadata =
                          {
                            t.metadata with
                            default =
                              (match requirement with
                              | Schema.Default v -> Some (to_json v)
                              | _ -> None);
                            description;
                            title =
                              Option.return
                                (Option.value ~default:(String.capitalize field)
                                   title);
                            examples = List.map ~f:to_json examples;
                          };
                        validation = { t.validation with maximum; minimum };
                      } );
                  ] )
            in
            match get_fields marks rest with
            | marks, None -> (marks, Some fields)
            | marks, Some rest -> (marks, Some (fields @ rest)))
        in
        let marks, fields = get_fields marks o in
        ( marks,
          Some
            {
              applicator_empty with
              properties = fields;
              additionalProperties = Some (Allowed false);
            } )
      | Schema.Tuple items ->
        let rec tuple_applicator : type u. ids -> u Schema.tuple -> ids * t list
            =
         fun marks -> function
          | Schema.ItemEnd -> (marks, [])
          | Schema.Item { schema; rest } ->
            let marks, head = of_schema marks schema in
            let marks, tail = tuple_applicator marks rest in
            (marks, head :: tail)
        in
        let marks, fields = tuple_applicator marks items in
        ( marks,
          Some { applicator_empty with items = Some (Tuple_schema fields) } )
      | Schema.Union { cases; _ } ->
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

open Base
include Json_schema_intf

let string_const_validation v =
  make_validation ~type_:(Either.first String) ~const:(`String v) ()

let string_const v =
  {
    applicator = make_applicator ();
    core = make_core ();
    metadata = make_metadata ();
    validation = string_const_validation v;
  }

type ids = (string, String.comparator_witness) Set.t
type date_tuple = int * int * int [@@deriving schema]

let make ?(applicator = make_applicator ()) ?(core = make_core ())
    ?(metadata = make_metadata ()) ?(validation = make_validation ()) () =
  make ~applicator ~core ~metadata ~validation

let of_schema schema =
  let id_name s = "#" ^ s in
  let rec of_schema : type u. ids -> u Schema.t -> ids * t =
   fun marks -> function
    | { id = None; descriptor; _ } -> of_descriptor marks descriptor
    | { id = Some id; _ } when Set.mem marks id ->
      (marks, make ~core:(make_core ~ref:(id_name id) ()) ())
    | { id = Some id; descriptor; _ } ->
      let marks = Set.add marks id in
      let marks, schema = of_descriptor marks descriptor in
      (marks, { schema with core = { schema.core with id = Some (id_name id) } })
  and of_descriptor : type u. ids -> u Schema.descriptor -> ids * t =
   fun marks schema ->
    let rec validation : type t. t Schema.descriptor -> validation = function
      | Schema.Any -> make_validation ()
      | Schema.Bool -> make_validation ~type_:(Either.first Boolean) ()
      | Schema.Date -> validation date_tuple_schema.descriptor
      | Fixpoint s -> validation (Lazy.force s)
      | Schema.Float -> make_validation ~type_:(Either.first Number) ()
      | Schema.Ignore -> make_validation ()
      | Schema.Int | Schema.Int64 ->
        make_validation ~type_:(Either.first Integer) ()
      | Schema.List _ -> make_validation ~type_:(Either.first Array) ()
      | Schema.Map { descriptor; _ } -> validation descriptor
      | Schema.Mapping _ -> make_validation ()
      | Schema.Null -> make_validation ~type_:(Either.first Null) ()
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
      | Schema.String -> make_validation ~type_:(Either.first String) ()
      | Schema.String_const v -> string_const_validation v
      | Schema.Time -> make_validation ~type_:(Either.first String) ()
      | Schema.Time_local -> make_validation ~type_:(Either.first String) ()
      | Schema.Object _ -> make_validation ~type_:(Either.first Object) ()
      | Schema.Tuple _ -> make_validation ~type_:(Either.first Array) ()
      | Schema.Union _ -> make_validation ()
    and applicator :
        type u. ids -> u Schema.descriptor -> ids * applicator option =
     fun marks -> function
      | Schema.Any -> (marks, Some (make_applicator ()))
      | Schema.Bool -> (marks, Some (make_applicator ()))
      | Schema.Date -> applicator marks date_tuple_schema.descriptor
      | Fixpoint s -> applicator marks (Lazy.force s)
      | Schema.Float | Schema.Ignore -> (marks, Some (make_applicator ()))
      | Schema.Int | Schema.Int64 -> (marks, Some (make_applicator ()))
      | Schema.List schema ->
        let marks, schema = of_schema marks schema in
        (marks, Some (make_applicator ~items:(Items_schema schema) ()))
      | Schema.Map { descriptor; _ } -> applicator marks descriptor
      | Schema.Mapping schema ->
        let marks, schema = of_schema marks schema in
        ( marks,
          Some
            (make_applicator ~additionalProperties:(Properties_schema schema) ())
        )
      | Schema.Null -> (marks, Some (make_applicator ()))
      | Schema.Nullable s -> applicator marks s.descriptor
      | Schema.String -> (marks, Some (make_applicator ()))
      | Schema.String_const _ -> (marks, Some (make_applicator ()))
      | Schema.Time -> (marks, Some (make_applicator ()))
      | Schema.Time_local -> (marks, Some (make_applicator ()))
      | Schema.Object o ->
        let rec get_fields :
            type u. ids -> u Schema.field -> ids * (string * t) list =
         fun marks -> function
          | Schema.FieldEnd -> (marks, [])
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
              } ->
            let marks, fields =
              match schema with
              | Inline schema | Inline_try schema -> (
                match applicator marks schema.descriptor with
                | marks, Some applicator -> (marks, applicator.properties)
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
            let marks, rest = get_fields marks rest in
            (marks, fields @ rest)
        in
        let marks, properties = get_fields marks o in
        ( marks,
          Some
            (make_applicator ~properties ~additionalProperties:(Allowed false)
               ()) )
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
        (marks, Some (make_applicator ~items:(Tuple_schema fields) ()))
      | Schema.Union { cases; key } ->
        let f (Schema.Case { name; schema; _ }) (marks, l) =
          let marks, schema = of_schema marks schema in
          let schema =
            match (key, schema) with
            | ( Some key,
                {
                  validation = { type_ = Some (Either.First Object); _ };
                  applicator = { properties; _ };
                  _;
                } ) ->
              {
                schema with
                applicator =
                  {
                    schema.applicator with
                    properties = (key, string_const name) :: properties;
                  };
              }
            | _ -> schema
          in
          (marks, schema :: l)
        in
        let marks, schemas = List.fold_right ~f ~init:(marks, []) cases in
        (marks, Some (make_applicator ~oneOf:schemas ()))
    in
    ( marks,
      {
        applicator = Option.value_exn (snd @@ applicator marks schema);
        core =
          make_core ~schema:"https://json-schema.org/draft/2019-09/schema" ();
        metadata = make_metadata ();
        validation = validation schema;
      } )
  in
  of_schema (Set.empty (module String)) schema |> snd

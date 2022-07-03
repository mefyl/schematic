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

let null =
  let validation = make_validation ~type_:(Either.first Null) () in
  make ~validation ()

let of_schema schema =
  let make ?(core = make_core ()) =
    let core =
      { core with schema = Some "https://json-schema.org/draft/2019-09/schema" }
    in
    make ~core
  in
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
   fun marks -> function
    | Schema.Any -> (marks, make ())
    | Schema.Bool ->
      let validation = make_validation ~type_:(Either.first Boolean) () in
      (marks, make ~validation ())
    | Schema.Date -> of_descriptor marks date_tuple_schema.descriptor
    | Fixpoint s -> of_descriptor marks (Lazy.force s)
    | Schema.Float ->
      let validation = make_validation ~type_:(Either.first Number) () in
      (marks, make ~validation ())
    | Schema.Ignore -> (marks, make ())
    | Schema.Int | Schema.Int64 ->
      let validation = make_validation ~type_:(Either.first Integer) () in
      (marks, make ~validation ())
    | Schema.List schema ->
      let marks, schema = of_schema marks schema in
      let applicator = make_applicator ~items:(Items_schema schema) ()
      and validation = make_validation ~type_:(Either.first Array) () in
      (marks, make ~applicator ~validation ())
    | Schema.Map { descriptor; _ } -> of_descriptor marks descriptor
    | Schema.Mapping schema ->
      let marks, schema = of_schema marks schema in
      let applicator =
        make_applicator ~additionalProperties:(Properties_schema schema) ()
      and validation = make_validation ~type_:(Either.first Object) () in
      (marks, make ~applicator ~validation ())
    | Schema.Null -> (marks, null)
    | Schema.Nullable s ->
      let marks, t = of_descriptor marks s.descriptor in
      let type_ =
        match t.validation.type_ with
        | None -> Some (Either.first Null)
        | Some (Either.First t) -> Some (Either.second [ t; Null ])
        | Some (Either.Second types) ->
          Some
            (Either.second
               (List.dedup_and_sort ~compare:Poly.compare (Null :: types)))
      and one_of =
        match t.applicator.one_of with [] -> [] | cases -> null :: cases
      in
      ( marks,
        {
          t with
          applicator = { t.applicator with one_of };
          validation = { t.validation with type_ };
        } )
    | Schema.String ->
      let validation = make_validation ~type_:(Either.first String) () in
      (marks, make ~validation ())
    | Schema.String_const v ->
      let validation = string_const_validation v in
      (marks, make ~validation ())
    | Schema.Time ->
      let validation = make_validation ~type_:(Either.first String) () in
      (marks, make ~validation ())
    | Schema.Time_local ->
      let validation = make_validation ~type_:(Either.first String) () in
      (marks, make ~validation ())
    | Schema.Object _ as o -> (
      let marks, properties =
        Result.ok_or_failwith
        @@ Schema.inline o marks
             {
               f =
                 (fun (type req t) marks
                      ({
                         description;
                         examples;
                         maximum;
                         minimum;
                         name;
                         requirement;
                         schema;
                         title;
                         _;
                       } :
                        (t, req) Schema.field) ->
                   let schema = Schema.extract_field_schema schema in
                   let marks, t = of_schema marks schema
                   and to_json v = Json.to_ezjsonm @@ Json.encode schema v in
                   ( marks,
                     ( name,
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
                                 (Option.value ~default:(String.capitalize name)
                                    title);
                             examples = List.map ~f:to_json examples;
                           };
                         validation = { t.validation with maximum; minimum };
                       } ) ));
             }
      and validation = make_validation ~type_:(Either.first Object) () in
      match
        List.map
          ~f:(fun properties ->
            make_applicator ~properties ~additionalProperties:(Allowed false) ())
          properties
      with
      | [ applicator ] -> (marks, make ~applicator ~validation ())
      | l ->
        let one_of = List.map ~f:(fun applicator -> make ~applicator ()) l in
        (marks, make ~applicator:(make_applicator ~one_of ()) ~validation ()))
    | Schema.Tuple items ->
      let marks, fields =
        let rec tuple_applicator : type u. ids -> u Schema.tuple -> ids * t list
            =
         fun marks -> function
          | Schema.ItemEnd -> (marks, [])
          | Schema.Item { schema; rest } ->
            let marks, head = of_schema marks schema in
            let marks, tail = tuple_applicator marks rest in
            (marks, head :: tail)
        in
        tuple_applicator marks items
      and validation = make_validation ~type_:(Either.first Array) () in
      ( marks,
        make
          ~applicator:(make_applicator ~items:(Tuple_schema fields) ())
          ~validation () )
    | Schema.Union { cases; key } ->
      let marks, schemas =
        let f marks (Schema.Case { name; schema; _ }) =
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
          (marks, schema)
        in
        List.fold_map ~f ~init:marks cases
      in
      let applicator = make_applicator ~one_of:schemas ()
      and validation =
        let f = function
          | { validation = { type_ = None; _ }; _ } -> []
          | { validation = { type_ = Some (Either.First t); _ }; _ } -> [ t ]
          | { validation = { type_ = Some (Either.Second t); _ }; _ } -> t
        in
        make_validation
          ~type_:(List.map ~f schemas |> List.concat |> Either.second)
          ()
      in
      (marks, make ~applicator ~validation ())
  in
  of_schema (Set.empty (module String)) schema |> snd

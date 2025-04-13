open Base
include Json_schema_intf

let string_const_validation v =
  make_validation ~type_:[ String ] ~const:(`String v) ()

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
  let validation = make_validation ~type_:[ Null ] () in
  make ~validation ()

module Inline_marks = Schema.Make_inline (State_monad.Make (struct
  type t = ids
end))

let of_schema schema =
  let make ?(core = make_core ()) =
    let core =
      { core with schema = Some "https://json-schema.org/draft/2019-09/schema" }
    in
    make ~core
  in
  let id_name s = "#" ^ s in
  let rec of_schema : type u. ids -> u Schema.t -> t * ids =
   fun marks -> function
    | { id = None; descriptor; _ } -> of_descriptor marks descriptor
    | { id = Some id; _ } when Set.mem marks id ->
      (make ~core:(make_core ~ref:(id_name id) ()) (), marks)
    | { id = Some id; descriptor; _ } ->
      let marks = Set.add marks id in
      let schema, marks = of_descriptor marks descriptor in
      ({ schema with core = { schema.core with id = Some (id_name id) } }, marks)
  and of_descriptor : type u. ids -> u Schema.descriptor -> t * ids =
   fun marks -> function
    | Any _ -> (make (), marks)
    | Bool ->
      let validation = make_validation ~type_:[ Boolean ] () in
      (make ~validation (), marks)
    | Coding { f } -> of_descriptor marks (f None)
    | Date -> of_descriptor marks date_tuple_schema.descriptor
    | Fixpoint s -> of_descriptor marks (Lazy.force s)
    | Float ->
      let validation = make_validation ~type_:[ Number ] () in
      (make ~validation (), marks)
    | Ignore -> (make (), marks)
    | Int | Int64 ->
      let validation = make_validation ~type_:[ Integer ] () in
      (make ~validation (), marks)
    | Json -> (make (), marks)
    | List schema ->
      let item, marks = of_schema marks schema in
      list_schema marks item
    | Sequence { item; _ } ->
      let item, marks = of_schema marks item in
      list_schema marks item
    | Map { descriptor; _ } -> of_descriptor marks descriptor
    | Mapping { item; _ } ->
      let schema, marks = of_schema marks item in
      let applicator =
        make_applicator ~additionalProperties:(Properties_schema schema) ()
      and validation = make_validation ~type_:[ Object ] () in
      (make ~applicator ~validation (), marks)
    | Null -> (null, marks)
    | Nullable s ->
      let t, marks = of_descriptor marks s.descriptor in
      let type_ =
        Null :: t.validation.type_ |> List.dedup_and_sort ~compare:Poly.compare
      and any_of =
        match t.applicator.any_of with [] -> [] | cases -> null :: cases
      in
      ( {
          t with
          applicator = { t.applicator with any_of };
          validation = { t.validation with type_ };
        },
        marks )
    | String ->
      let validation = make_validation ~type_:[ String ] () in
      (make ~validation (), marks)
    | String_const v ->
      let validation = string_const_validation v in
      (make ~validation (), marks)
    | Time ->
      let validation = make_validation ~type_:[ String ] () in
      (make ~validation (), marks)
    | Time_local ->
      let validation = make_validation ~type_:[ String ] () in
      (make ~validation (), marks)
    | Object _ as o -> (
      let properties, marks =
        let f =
          Result.ok_or_failwith
          @@ Inline_marks.schema o
               {
                 f =
                   (fun (type req t)
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
                          (t, req) Schema.field) marks ->
                     let schema = Schema.field_schema schema in
                     let t, marks = of_schema marks schema
                     and to_json v = Json.to_ezjsonm @@ Json.encode schema v in
                     ( ( name,
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
                                   (Option.value
                                      ~default:(String.capitalize name) title);
                               examples = List.map ~f:to_json examples;
                             };
                           validation = { t.validation with maximum; minimum };
                         } ),
                       marks ));
                 filter = None;
               }
        in
        f marks
      and validation = make_validation ~type_:[ Object ] () in
      match
        List.map
          ~f:(fun properties ->
            make_applicator ~properties ~additionalProperties:(Allowed false) ())
          properties
      with
      | [ applicator ] -> (make ~applicator ~validation (), marks)
      | l ->
        let any_of = List.map ~f:(fun applicator -> make ~applicator ()) l in
        (make ~applicator:(make_applicator ~any_of ()) ~validation (), marks))
    | Schema.Tuple { items; _ } ->
      let fields, marks =
        let rec tuple_applicator : type u. ids -> u Schema.tuple -> t list * ids
            =
         fun marks -> function
          | Schema.ItemEnd -> ([], marks)
          | Schema.Item { schema; rest } ->
            let head, marks = of_schema marks schema in
            let tail, marks = tuple_applicator marks rest in
            (head :: tail, marks)
        in
        tuple_applicator marks items
      and validation = make_validation ~type_:[ Array ] () in
      ( make
          ~applicator:(make_applicator ~items:(Tuple_schema fields) ())
          ~validation (),
        marks )
    | Schema.Union { cases; key; _ } ->
      let marks, schemas =
        let f marks (Schema.Case { name; schema; _ }) =
          let schema, marks = of_schema marks schema in
          let schema =
            match (key, schema) with
            | ( Some key,
                {
                  validation = { type_ = [ Object ]; _ };
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
      let applicator = make_applicator ~any_of:schemas ()
      and validation =
        let f = function { validation = { type_; _ }; _ } -> type_ in
        make_validation
          ~type_:
            (List.map ~f schemas |> List.concat
            |> List.dedup_and_sort ~compare:Poly.compare)
          ()
      in
      (make ~applicator ~validation (), marks)
  and list_schema marks item =
    let applicator = make_applicator ~items:(Items_schema item) ()
    and validation = make_validation ~type_:[ Array ] () in
    (make ~applicator ~validation (), marks)
  in
  of_schema (Set.empty (module String)) schema |> fst

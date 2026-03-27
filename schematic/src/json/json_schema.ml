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

type date_tuple = int * int * int [@@deriving schema]

let make ?(applicator = make_applicator ()) ?(core = make_core ())
    ?(metadata = make_metadata ()) ?(validation = make_validation ()) () =
  make ~applicator ~core ~metadata ~validation

let null =
  let validation = make_validation ~type_:[ Null ] () in
  make ~validation ()

let of_schema ?(defs = false) schema =
  let open struct
    type state = {
      marks : (string, String.comparator_witness) Set.t;
      defs : (string * t) list;
    }
  end in
  let empty_state = { marks = Set.empty (module String); defs = [] } in
  let module Inline_marks = Schema.Make_inline (State_monad.Make (struct
    type t = state
  end)) in
  let make ?(core = make_core ()) =
    let core =
      { core with schema = Some "https://json-schema.org/draft/2019-09/schema" }
    in
    make ~core
  in
  let id_name s = "#" ^ s in
  let with_documentation documentation schema =
    match documentation with
    | None -> schema
    | Some doc ->
      { schema with metadata = { schema.metadata with description = Some doc } }
  in
  let rec of_schema : type u. state -> u Schema.t -> t * state =
   fun state -> function
     | { id = None; documentation; descriptor; _ } ->
       let schema, state = of_descriptor state descriptor in
       (with_documentation documentation schema, state)
     | { id = Some id; _ } when Set.mem state.marks id ->
       let ref_str = if defs then "#/$defs/" ^ id else id_name id in
       (make ~core:(make_core ~ref:ref_str ()) (), state)
     | { id = Some id; documentation; descriptor; _ } ->
       let state = { state with marks = Set.add state.marks id } in
       let schema, state = of_descriptor state descriptor in
       let schema = with_documentation documentation schema in
       if defs then
         let state = { state with defs = (id, schema) :: state.defs } in
         let schema = make ~core:(make_core ~ref:("#/$defs/" ^ id) ()) () in
         (schema, state)
       else
         let schema =
           { schema with core = { schema.core with id = Some (id_name id) } }
         in
         (schema, state)
  and of_descriptor : type u. state -> u Schema.descriptor -> t * state =
   fun state -> function
     | Any _ -> (make (), state)
     | Bool ->
       let validation = make_validation ~type_:[ Boolean ] () in
       (make ~validation (), state)
     | Coding { f } -> of_descriptor state (f None)
     | Date -> of_descriptor state date_tuple_schema.descriptor
     | Fixpoint s -> of_descriptor state (Lazy.force s)
     | Float ->
       let validation = make_validation ~type_:[ Number ] () in
       (make ~validation (), state)
     | Ignore -> (make (), state)
     | Int | Int64 ->
       let validation = make_validation ~type_:[ Integer ] () in
       (make ~validation (), state)
     | Json -> (make (), state)
     | List schema ->
       let item, state = of_schema state schema in
       list_schema state item
     | Map { descriptor; _ } -> of_descriptor state descriptor
     | Mapping { item; _ } ->
       let schema, state = of_schema state item in
       let applicator =
         make_applicator ~additionalProperties:(Properties_schema schema) ()
       and validation = make_validation ~type_:[ Object ] () in
       (make ~applicator ~validation (), state)
     | Null -> (null, state)
     | Nullable s ->
       let t, state = of_descriptor state s.descriptor in
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
         state )
     | Sequence { item; _ } ->
       let item, state = of_schema state item in
       list_schema state item
     | String ->
       let validation = make_validation ~type_:[ String ] () in
       (make ~validation (), state)
     | String_const v ->
       let validation = string_const_validation v in
       (make ~validation (), state)
     | Time ->
       let validation = make_validation ~type_:[ String ] () in
       (make ~validation (), state)
     | Time_local ->
       let validation = make_validation ~type_:[ String ] () in
       (make ~validation (), state)
     | Object _ as o -> (
       let properties, state =
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
                        (t, req) Schema.field)
                      state
                    ->
                      let schema = Schema.field_schema schema in
                      let t, state = of_schema state schema
                      and to_json v =
                        Json_coding.to_ezjsonm @@ Json_coding.encode schema v
                      in
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
                        state ));
                  filter = None;
                }
         in
         f state
       and validation = make_validation ~type_:[ Object ] () in
       match
         List.map
           ~f:(fun properties ->
             make_applicator ~properties ~additionalProperties:(Allowed false)
               ())
           properties
       with
       | [ applicator ] -> (make ~applicator ~validation (), state)
       | l ->
         let any_of = List.map ~f:(fun applicator -> make ~applicator ()) l in
         (make ~applicator:(make_applicator ~any_of ()) ~validation (), state))
     | Schema.Tuple { items; _ } ->
       let fields, state =
         let rec tuple_applicator : type u.
             state -> u Schema.tuple -> t list * state =
          fun state -> function
            | Schema.ItemEnd -> ([], state)
            | Schema.Item { schema; rest } ->
              let head, state = of_schema state schema in
              let tail, state = tuple_applicator state rest in
              (head :: tail, state)
         in
         tuple_applicator state items
       and validation = make_validation ~type_:[ Array ] () in
       ( make
           ~applicator:(make_applicator ~items:(Tuple_schema fields) ())
           ~validation (),
         state )
     | Schema.Union { cases; key; _ } ->
       let state, schemas =
         let f state (Schema.Case { name; schema; _ }) =
           let schema, state = of_schema state schema in
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
           (state, schema)
         in
         List.fold_map ~f ~init:state cases
       in
       let applicator = make_applicator ~any_of:schemas ()
       and validation =
         let f = function { validation = { type_; _ }; _ } -> type_ in
         make_validation
           ~type_:
             (List.map ~f schemas
             |> List.concat
             |> List.dedup_and_sort ~compare:Poly.compare)
           ()
       in
       (make ~applicator ~validation (), state)
  and list_schema state item =
    let applicator = make_applicator ~items:(Items_schema item) ()
    and validation = make_validation ~type_:[ Array ] () in
    (make ~applicator ~validation (), state)
  in
  let result, state = of_schema empty_state schema in
  if defs then
    { result with core = { result.core with defs = List.rev state.defs } }
  else result

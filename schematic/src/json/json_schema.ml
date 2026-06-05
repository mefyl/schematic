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

let of_schema ?(reference = `Ids) (schema : _ Schema.t) =
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
      { core with schema = Some "https://json-schema.org/draft/2020-12/schema" }
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
  let rec of_schema : type u. state -> u Schema.t -> t * type_ list * state =
   fun state -> function
     | { id = None; documentation; descriptor; _ } ->
       let schema, types, state = of_descriptor state descriptor in
       (with_documentation documentation schema, types, state)
     | { id = Some id; _ } when Set.mem state.marks id ->
       let ref_str =
         match reference with `Defs -> "#/$defs/" ^ id | `Ids -> id_name id
       in
       (make ~core:(make_core ~ref:ref_str ()) (), [], state)
     | { id = Some id; documentation; descriptor; _ } -> (
       let state = { state with marks = Set.add state.marks id } in
       let schema, types, state = of_descriptor state descriptor in
       let schema = with_documentation documentation schema in
       match reference with
       | `Defs ->
         let state = { state with defs = (id, schema) :: state.defs } in
         let schema = make ~core:(make_core ~ref:("#/$defs/" ^ id) ()) () in
         (schema, types, state)
       | `Ids ->
         let schema =
           { schema with core = { schema.core with anchor = Some id } }
         in
         (schema, types, state))
  and of_descriptor : type u.
      state -> u Schema.descriptor -> t * type_ list * state =
   fun state -> function
     | Any _ -> (make (), [], state)
     | Bool ->
       let type_ = [ Boolean ] in
       let validation = make_validation ~type_ () in
       (make ~validation (), type_, state)
     | Coding { f } -> of_descriptor state (f None)
     | Date -> of_descriptor state date_tuple_schema.descriptor
     | Fixpoint s -> of_descriptor state (Lazy.force s)
     | Float ->
       let type_ = [ Number ] in
       let validation = make_validation ~type_ () in
       (make ~validation (), type_, state)
     | Ignore -> (make (), [], state)
     | Int | Int64 ->
       let type_ = [ Integer ] in
       let validation = make_validation ~type_ () in
       (make ~validation (), type_, state)
     | Json -> (make (), [], state)
     | List schema ->
       let item, _, state = of_schema state schema in
       list_schema state item
     | Map { descriptor; _ } -> of_descriptor state descriptor
     | Mapping { item; _ } ->
       let schema, _, state = of_schema state item in
       let type_ = [ Object ] in
       let applicator =
         make_applicator ~additionalProperties:(Properties_schema schema) ()
       and validation = make_validation ~type_ () in
       (make ~applicator ~validation (), type_, state)
     | Null -> (null, [ Null ], state)
     | Nullable s ->
       let t, inner_types, state = of_descriptor state s.descriptor in
       let any_of =
         match t.applicator.any_of with [] -> [] | cases -> null :: cases
       in
       let type_ =
         Null :: inner_types |> List.dedup_and_sort ~compare:Poly.compare
       in
       ( {
           t with
           applicator = { t.applicator with any_of };
           validation = { t.validation with type_ };
         },
         type_,
         state )
     | Sequence { item; _ } ->
       let item, _, state = of_schema state item in
       list_schema state item
     | String ->
       let type_ = [ String ] in
       let validation = make_validation ~type_ () in
       (make ~validation (), type_, state)
     | String_const v ->
       let type_ = [ String ] in
       let validation = string_const_validation v in
       (make ~validation (), type_, state)
     | Time ->
       let type_ = [ String ] in
       let validation = make_validation ~type_ () in
       (make ~validation (), type_, state)
     | Time_local ->
       let type_ = [ String ] in
       let validation = make_validation ~type_ () in
       (make ~validation (), type_, state)
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
                      let t, _, state = of_schema state schema
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
       and type_ = [ Object ] in
       let validation = make_validation ~type_ () in
       match
         List.map
           ~f:(fun properties ->
             make_applicator ~properties ~additionalProperties:(Allowed false)
               ())
           properties
       with
       | [ applicator ] -> (make ~applicator ~validation (), type_, state)
       | l ->
         let any_of = List.map ~f:(fun applicator -> make ~applicator ()) l in
         ( make ~applicator:(make_applicator ~any_of ()) ~validation (),
           type_,
           state ))
     | Schema.Tuple { items; _ } ->
       let fields, state =
         let rec tuple_applicator : type u.
             state -> u Schema.tuple -> t list * state =
          fun state -> function
            | Schema.ItemEnd -> ([], state)
            | Schema.Item { schema; rest } ->
              let head, _, state = of_schema state schema in
              let tail, state = tuple_applicator state rest in
              (head :: tail, state)
         in
         tuple_applicator state items
       and type_ = [ Array ] in
       let validation = make_validation ~type_ () in
       ( make ~applicator:(make_applicator ~prefixItems:fields ()) ~validation (),
         type_,
         state )
     | Schema.Union { cases; key; _ } ->
       let state, schemas_with_types =
         let f state (Schema.Case { name; schema; _ }) =
           let schema, types, state = of_schema state schema in
           let schema =
             match (key, schema) with
             | ( Some key,
                 {
                   validation = { type_ = [ Object ]; _ };
                   applicator = { properties; any_of; _ };
                   _;
                 } ) ->
               let key_schema = string_const name in
               let any_of =
                 List.map any_of ~f:(fun item ->
                     match item.applicator.additionalProperties with
                     | Allowed false ->
                       {
                         item with
                         applicator =
                           {
                             item.applicator with
                             properties =
                               (key, key_schema) :: item.applicator.properties;
                           };
                       }
                     | _ -> item)
               in
               {
                 schema with
                 applicator =
                   {
                     schema.applicator with
                     properties = (key, key_schema) :: properties;
                     any_of;
                   };
               }
             | _ -> schema
           in
           (state, (schema, types))
         in
         List.fold_map ~f ~init:state cases
       in
       let schemas = List.map ~f:fst schemas_with_types in
       let applicator = make_applicator ~any_of:schemas () in
       let validation =
         make_validation
           ~type_:
             (List.concat_map schemas_with_types ~f:snd
             |> List.dedup_and_sort ~compare:Poly.compare)
           ()
       in
       (make ~applicator ~validation (), validation.type_, state)
  and list_schema state item =
    let type_ = [ Array ] in
    let applicator = make_applicator ~items:item ()
    and validation = make_validation ~type_ () in
    (make ~applicator ~validation (), type_, state)
  in
  match reference with
  | `Defs ->
    let result, _, state =
      of_descriptor empty_state schema.descriptor |> fun (s, types, st) ->
      (with_documentation schema.documentation s, types, st)
    in
    { result with core = { result.core with defs = List.rev state.defs } }
  | `Ids -> of_schema empty_state schema |> fun (s, _, _) -> s

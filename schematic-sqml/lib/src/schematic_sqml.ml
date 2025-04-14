open Acid

let rec atomic_type_of_descriptor :
    type t.
    ?id:string ->
    t Schematic.Schema.descriptor ->
    t Sqml.Expression.Column.Type.atomic =
 fun ?id descriptor ->
  let fail name =
    Fmt.failwith "sqml: %acannot bind %s descriptor@ @[%a@]"
      Fmt.(option (const string "schema " ++ string ++ const char ':' ++ sp))
      id name Schematic.Schema.pp_descriptor descriptor
  in
  match descriptor with
  | Any _ -> fail "Any"
  | Bool -> Bool
  | Coding { f } -> atomic_type_of_descriptor ?id (f None)
  | Float -> Float
  | Date ->
    let decode = Fn.compose Result.return Timmy.Date.to_tuple
    and encode = Timmy.Date.of_tuple_exn ~here:[%here] in
    Custom { decode; encode; repr = Date }
  | Fixpoint s -> atomic_type_of_descriptor ?id (Lazy.force s)
  | Int -> Int
  | Ignore -> fail "Ignore"
  | Int64 -> Bigint
  | Json -> Json
  | List _ -> fail "List"
  | Sequence _ -> fail "Sequence"
  | Null -> fail "Null"
  | String -> Text
  | Time ->
    Custom
      {
        repr = Time;
        encode = Timmy.Time.of_ptime;
        decode = Fn.compose Result.return Timmy.Time.to_ptime;
      }
  | Time_local -> fail "Time_local (FIXME)"
  | Map { decode; encode; descriptor } ->
    Custom { decode; encode; repr = atomic_type_of_descriptor descriptor }
  | Mapping _ -> fail "Mapping"
  | Nullable schema -> Nullable (atomic_type_of_descriptor schema.descriptor)
  | String_const _ -> fail "String_const"
  | Object _ -> fail "Object"
  | Tuple _ -> fail "Tuple"
  | Union _ -> fail "Union"

and atomic_type_of_schema Schematic.Schema.{ descriptor; id; _ } =
  atomic_type_of_descriptor ?id descriptor

let rec types_of_fields :
    type l.
    l Hlist.t Schematic.Schema.fields -> l Sqml.Expression.Column.Type.List.t =
  function
  | Field { field = { name; requirement; schema; _ }; rest } -> (
    let rest = types_of_fields rest in
    match requirement with
    | Default _ ->
      (name, atomic_type_of_schema (Schematic.Schema.field_schema schema))
      :: rest
    | Optional ->
      ( name,
        Nullable (atomic_type_of_schema (Schematic.Schema.field_schema schema))
      )
      :: rest
    | Required ->
      (name, atomic_type_of_schema (Schematic.Schema.field_schema schema))
      :: rest
    | Computed | Computed_optional ->
      Fmt.failwith "sqml: cannot bind computed field %S" name)
  | FieldEnd -> []

let rec hlist_of_fields :
    type l. l Hlist.t Schematic.Schema.fields -> l Hlist.t -> l Hlist.t =
  function
  | Field { field = _; rest } ->
    let rest = hlist_of_fields rest in
    fun (hd :: tl) -> hd :: rest tl
  | FieldEnd -> fun [] -> []

let rec type_of_descriptor :
    type t.
    ?id:string ->
    t Schematic.Schema.descriptor ->
    t Sqml.Expression.Column.Type.t =
 fun ?id descriptor ->
  match descriptor with
  | Any _ | Bool | Float | Date | Int | Ignore | Int64 | Json | List _ | Map _
  | Mapping _ | Null | Nullable _ | Sequence _ | String | Time | Time_local
  | String_const _ | Tuple _ | Union _ ->
    Atomic (atomic_type_of_descriptor descriptor)
  | Coding { f } -> type_of_descriptor ?id (f None)
  | Fixpoint s -> type_of_descriptor ?id (Lazy.force s)
  | Object { decode; encode; fields } ->
    let columns = types_of_fields fields
    and hlist_of_fields = hlist_of_fields fields in
    Composite
      {
        columns;
        decode = (fun columns -> decode columns);
        encode = (fun o -> encode o |> hlist_of_fields);
      }

and type_of_schema Schematic.Schema.{ descriptor; id; _ } =
  type_of_descriptor ?id descriptor

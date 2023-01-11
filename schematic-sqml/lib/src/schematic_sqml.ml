open Acid

let rec type_of_descriptor :
    type t.
    ?id:string ->
    t Schematic.Schema.descriptor ->
    t Sqml.Expression.Column.Type.t =
 fun ?id descriptor ->
  let fail name =
    Fmt.failwith "sqml: %acannot bind %s descriptor@ @[%a@]"
      Fmt.(option (const string "schema " ++ string ++ const char ':' ++ sp))
      id name Schematic.Schema.pp_descriptor descriptor
  in
  match descriptor with
  | Any _ -> fail "Any"
  | Bool -> Bool
  | Float -> Float
  | Date ->
    let decode = Fn.compose Result.return Timmy.Date.to_tuple
    and encode = Timmy.Date.of_tuple_exn ~here:[%here] in
    Custom { decode; encode; repr = Date }
  | Fixpoint s -> type_of_descriptor ?id (Lazy.force s)
  | Int -> Int
  | Ignore -> fail "Ignore"
  | Int64 -> Bigint
  | Json -> Json
  | List _ -> fail "List"
  | Null -> fail "Null"
  | String -> Text
  | Time ->
    Sqml.Column.Type.Custom
      {
        repr = Time;
        encode = Timmy.Time.of_ptime;
        decode = Fn.compose Result.return Timmy.Time.to_ptime;
      }
  | Time_local -> fail "Time_local (FIXME)"
  | Map { decode; encode; descriptor } ->
    Custom { decode; encode; repr = type_of_descriptor descriptor }
  | Mapping _ -> fail "Mapping"
  | Nullable schema -> Nullable (type_of_descriptor schema.descriptor)
  | String_const _ -> fail "String_const"
  | Object _ -> fail "Object"
  | Tuple _ -> fail "Tuple"
  | Union _ -> fail "Union"

and type_of_schema Schematic.Schema.{ descriptor; id; _ } =
  type_of_descriptor ?id descriptor

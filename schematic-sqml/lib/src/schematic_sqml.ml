open Acid

let rec type_of_descriptor :
    type t. t Schematic.Schema.descriptor -> t Sqml.Expression.Column.Type.t =
  function
  | Any -> Json
  | Bool -> Bool
  | Float -> Float
  | Date ->
    let decode = Fn.compose Result.return Timmy.Date.to_tuple
    and encode = Timmy.Date.of_tuple_exn ~here:[%here] in
    Custom { decode; encode; repr = Date }
  | Fixpoint s -> type_of_descriptor (Lazy.force s)
  | Int -> Int
  | Ignore -> failwith "schematic-sqml: cannot bind Ignore schema"
  | Int64 -> Bigint
  | List _ -> failwith "schematic-sqml: cannot bind List schema"
  | Null -> failwith "schematic-sqml: cannot bind Null schema"
  | String -> Text
  | Time ->
    Sqml.Column.Type.Custom
      {
        repr = Time;
        encode = Timmy.Time.of_ptime;
        decode = Fn.compose Result.return Timmy.Time.to_ptime;
      }
  | Time_local -> failwith "FIXME"
  | Map { decode; encode; descriptor } ->
    Custom { decode; encode; repr = type_of_descriptor descriptor }
  | Mapping _ -> failwith "schematic-sqml: cannot bind Mapping schema"
  | Nullable schema -> Nullable (type_of_descriptor schema.descriptor)
  | String_const _ -> failwith "schematic-sqml: cannot bind String_const schema"
  | Object _ -> failwith "schematic-sqml: cannot bind object schema"
  | Tuple _ -> failwith "schematic-sqml: cannot bind tuple schema"
  | Union _ -> failwith "schematic-sqml: cannot bind tuple schema"

and type_of_schema Schematic.Schema.{ descriptor; _ } =
  type_of_descriptor descriptor

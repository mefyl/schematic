open Base
open C_intf

let rec pp_swift_type f c =
  Logs.debug (fun m -> m "pp_swift_type: %a" pp_c c);
  let open Fmt in
  let array : c Fmt.t = fun f -> Fmt.fmt "Array<%a>" f pp_swift_type in
  let pp =
    let tup_pp c =
      tuple_members c
      |> Option.map ~f:(fun members ->
             const char '('
             ++ const (list ~sep:(const string ", ") pp_swift_type) members
             ++ const char ')')
    in
    let array_pp c =
      array_element c |> Option.map ~f:(fun c -> const array c)
    in
    let option_pp c =
      option_element c
      |> Option.map ~f:(fun c -> const pp_swift_type c ++ const char '?')
    in
    let other_pp = function
      | (`Tuple _ | `Array _ | `Option _) as c ->
        failwith "%a should already have been handled" pp_c c
      | `Void -> const string "Void"
      | `Unit -> const string "Unit"
      | `String -> const string "String"
      | `Double -> const string "Double"
      | `Bool -> const string "Bool"
      | `Int -> const string "Int"
      | `Int64 -> const string "Int64"
      | `Json -> const string "String"
      | `Time -> const string "SchematicTime"
      | `Atom (s, _) -> const string (Swiftify.Type.id s)
      | `Mapped front_facing -> const pp_swift_type front_facing
      | (`Struct _ | `Variant _ | `Poly_variant _) as c ->
        failwith "tried to refer to %a without going through an `Atom" pp_c c
    in
    [ tup_pp; array_pp; option_pp ]
    |> List.fold
         ~f:(fun acc f -> match acc with Some x -> Some x | None -> f c)
         ~init:None
    |> function
    | Some pp -> pp
    | None -> other_pp c
  in
  pp f ()

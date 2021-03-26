open Base
open Utils
include Schema_intf

let required : type a b. (a, b) field_requirement -> bool = function
  | Required -> true
  | _ -> false

let map cons decons { descriptor; _ } =
  { id = None; descriptor = Map (cons, decons, descriptor) }

let rec pp_descriptor : type t kind. (t, kind) descriptor Fmt.t =
 fun f d ->
  let fmt =
    let open Fmt in
    let indent = 2 in
    match d with
    | Any -> const string "any"
    | Bool -> const string "bool"
    | Date -> const string "date"
    | Float -> const string "int"
    | Ignore -> const string "ignore"
    | Int -> const string "int"
    | Int64 -> const string "int64"
    | List schema ->
      box ~indent
      @@ const string "[" ++ sp
         ++ (box @@ const pp schema)
         ++ sp ++ const string "]"
    | Map (_, _, d) -> const pp_descriptor d
    | Mapping schema -> const pp schema
    | Null -> const string "()"
    | Nullable s ->
      const string "Nullable ("
      ++ const pp_descriptor s.descriptor
      ++ const string ")"
    | String -> const string "string"
    | String_const v -> escape v
    | Time -> const string "time"
    | Time_local -> const string "local-time"
    | Object _ -> const string "FIXME OBJECT"
    | Tuple _ -> const string "FIXME TUPLE"
    | Union _ -> const string "FIXME UNION"
  in
  fmt f ()

and pp : type t. t schema Fmt.t =
 fun f { descriptor; _ } -> pp_descriptor f descriptor

let extract_field_schema = function
  | Inline schema -> (true, schema)
  | Outline schema -> (false, schema)

type 'res inline = { f : 't. 't field -> 'res }

let rec inline : type t. (t, _) descriptor -> 'res inline -> 'res =
 fun d f ->
  match d with
  | Map (_, _, d) -> inline d f
  | Object o -> f.f o
  | s -> Fmt.failwith "inline field is not an object: %a" pp_descriptor s

type 'res inline_v = { f : 't. 't field -> 't -> 'res }

let rec inline_v : type t. (t, _) descriptor -> t -> 'res inline_v -> 'res =
 fun d v f ->
  match d with
  | Map (_, e, d) -> inline_v d (e v) f
  | Object o -> f.f o v
  | s -> Fmt.failwith "inline field is not an object: %a" pp_descriptor s

let rec object_fields : type t. t field -> string list = function
  | End -> []
  | Field { requirement = Default _ | Optional; rest; _ } -> object_fields rest
  | Field { rest; schema = Inline schema; _ } ->
    descriptor_fields schema.descriptor @ object_fields rest
  | Field { field; rest; _ } -> field :: object_fields rest

and descriptor_fields : type t. (t, _) descriptor -> string list = function
  | Map (_, _, d) -> descriptor_fields d
  | Object fields -> object_fields fields
  | s -> Fmt.failwith "inline field is not an object: %a" pp_descriptor s

let assoc_compare compare l r =
  let f (key, l) =
    match List.Assoc.find ~equal:String.equal r key with
    | None -> Some 1
    | Some r -> (
      match compare l r with
      | 0 -> None
      | v -> Some v)
  in
  Option.value ~default:0 @@ List.find_map ~f l

let compare schema =
  let rec compare_json l r =
    match (l, r) with
    | `Null, `Null -> 0
    | `Null, _ -> -1
    | _, `Null -> 1
    | `Bool l, `Bool r -> Bool.compare l r
    | `Bool _, _ -> -1
    | _, `Bool _ -> 1
    | `Float l, `Float r -> Float.compare l r
    | `Float _, _ -> -1
    | _, `Float _ -> 1
    | `String l, `String r -> String.compare l r
    | `String _, _ -> -1
    | _, `String _ -> 1
    | `A l, `A r -> List.compare compare_json l r
    | `A _, _ -> -1
    | _, `A _ -> 1
    | `O l, `O r -> assoc_compare compare_json l r
  in
  let rec compare : type t kind. (t, kind) descriptor -> t -> t -> int =
    function
    | Any -> compare_json
    | Bool -> Base.Bool.compare
    | Date -> Base.Poly.compare
    | Float -> Base.Poly.compare
    | Ignore -> Base.Poly.compare
    | Int
    | Int64 ->
      Base.Poly.compare
    | List schema -> Base.List.compare (compare schema.descriptor)
    | Map (_, map, v) -> fun l r -> compare v (map l) (map r)
    | Mapping schema -> assoc_compare (compare schema.descriptor)
    | Null -> fun () () -> 0
    | Nullable s -> fun l r -> Base.Option.compare (compare s.descriptor) l r
    | String -> String.compare
    | String_const _ -> fun () () -> 0
    | Time -> Ptime.compare
    | Time_local -> (
      fun (l, lz) (r, rz) ->
        match Int.compare lz rz with
        | 0 -> Ptime.compare l r
        | v -> v)
    | Object fields ->
      let rec compare_fields : type t. t field -> t -> t -> int = function
        | Field { schema; requirement; rest; _ } ->
          let _, schema = extract_field_schema schema in
          let f
              (type field req)
              (schema : field schema)
              (requirement : (field, req) field_requirement)
              ((l : req), lr)
              ((r : req), rr) =
            let c =
              match requirement with
              | Required -> compare schema.descriptor l r
              | Default _ -> compare schema.descriptor l r
              | Optional -> Option.compare (compare schema.descriptor) l r
            in
            match c with
            | 0 -> compare_fields rest lr rr
            | v -> v
          in
          f schema requirement
        | End -> fun () () -> 0
      in
      compare_fields fields
    | Tuple { schema; rest } -> (
      fun (l, lr) (r, rr) ->
        match compare schema.descriptor l r with
        | 0 -> compare rest lr rr
        | v -> v)
    | Union { cases; _ } ->
      fun l r ->
        let f (Case { schema; encoder; _ }) =
          match (encoder l, encoder r) with
          | None, None -> None
          | Some _, None -> Some (-1)
          | None, Some _ -> Some 1
          | Some l, Some r -> Some (compare schema.descriptor l r)
        in
        Option.value ~default:0 @@ List.find_map ~f cases
  in
  compare schema.descriptor

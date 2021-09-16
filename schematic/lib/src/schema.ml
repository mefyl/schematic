open Utils
include Schema_intf

let required : type a b. (a, b) requirement -> bool = function
  | Required -> true
  | _ -> false

let map cons decons { descriptor; _ } =
  { id = None; descriptor = Map (cons, decons, descriptor); parametric = None }

let extract_field_schema = function
  | Inline schema -> (true, schema)
  | Outline schema -> (false, schema)

let rec pp_descriptor : type u. u descriptor Fmt.t =
 fun f d ->
  let fmt =
    let open Fmt in
    let indent = 2 in
    match d with
    | Any -> const string "Any"
    | Bool -> const string "Bool"
    | Date -> const string "Date"
    | Float -> const string "Float"
    | Ignore -> const string "Ignore"
    | Int -> const string "Int"
    | Int64 -> const string "Int64"
    | List schema ->
      box ~indent
      @@ const string "[" ++ sp
         ++ (box @@ const pp schema)
         ++ sp ++ const string "]"
    | Map (_, _, d) -> const pp_descriptor d
    | Mapping schema -> const pp schema
    | Null -> const string "Null"
    | Nullable s ->
      const string "Nullable ("
      ++ const pp_descriptor s.descriptor
      ++ const string ")"
    | String -> const string "String"
    | String_const v -> escape v
    | Time -> const string "Time"
    | Time_local -> const string "Time_local"
    | Object fields ->
      box
      @@ (hvbox ~indent
         @@ (const string "{" ++ sp ++ const (hvbox pp_fields) fields))
         ++ sp ++ const string "}"
    | Tuple _ -> const string "FIXME TUPLE"
    | Union { cases; _ } ->
      box
      @@ hvbox ~indent
           (const string "Union [" ++ sp
           ++ (hvbox @@ const (hvbox (list ~sep:comma pp_case)) cases))
         ++ sp ++ const string "]"
  in
  fmt f ()

and pp_case : type u. u case Fmt.t =
 fun f (Case { name; schema; _ }) ->
  Fmt.pf f "@[@[<hv>{@ name: %S,@ schema: %a@ @]}@]" name pp schema

and pp_fields : type t. t field Fmt.t =
 fun fmt -> function
  | FieldEnd -> ()
  | Field { field = name; schema; rest; _ } ->
    let _, schema = extract_field_schema schema in
    Fmt.(
      const string name ++ const string ": "
      ++ const pp_descriptor schema.descriptor
      ++ comma ++ const pp_fields rest)
      fmt ()

and pp : type u. u t Fmt.t =
 fun f { descriptor; _ } -> pp_descriptor f descriptor

type 'res inline = { f : 't. 't field -> 'res }

let rec inline : type t. t descriptor -> 'res inline -> 'res list =
 fun d f ->
  match d with
  | Ignore -> [ f.f FieldEnd ]
  | Map (_, _, d) -> inline d f
  | Object o -> [ f.f o ]
  | Union { cases; _ } ->
    List.map ~f:(fun (Case { schema; _ }) -> inline schema.descriptor f) cases
    |> List.concat
  | s -> Fmt.failwith "inline field is not an object: %a" pp_descriptor s

type 'res inline_v = { f : 't. 't field -> 't -> 'res }

let rec inline_v : type t. t descriptor -> t -> 'res inline_v -> 'res =
 fun d v f ->
  match d with
  | Ignore -> f.f FieldEnd v
  | Map (_, e, d) -> inline_v d (e v) f
  | Object o -> f.f o v
  | Union { cases; _ } -> (
    let f (Case { encoder; schema; _ }) =
      match encoder v with
      | None -> None
      | Some v -> Some (inline_v schema.descriptor v f)
    in
    match List.find_map ~f cases with
    | None -> Fmt.failwith "no union cased matched in inline_v"
    | Some v -> v)
  | s -> Fmt.failwith "inlined value field is not an object: %a" pp_descriptor s

(** [descriptor_fields descriptor] is a list of alternative mandatory fields
    name list required to decode an object decribed by [descriptor]. *)
let descriptor_fields =
  let rec object_fields : type t. t field -> string list list = function
    | FieldEnd -> [ [] ]
    | Field { requirement = Default _ | Optional; rest; _ } ->
      object_fields rest
    | Field { rest; schema = Inline schema; _ } ->
      let open Base.Sequence in
      cartesian_product
        (of_list @@ descriptor_fields schema.descriptor)
        (of_list @@ object_fields rest)
      |> map ~f:(fun (l, r) -> l @ r)
      |> to_list
    | Field { field; rest; _ } ->
      List.map ~f:(List.cons field) @@ object_fields rest
  and descriptor_fields : type t. t descriptor -> string list list =
   fun d -> inline d { f = object_fields } |> List.concat
  in
  descriptor_fields

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
  let rec compare : type u. u descriptor -> u -> u -> int = function
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
      let rec compare_fields : type u. u field -> u -> u -> int = function
        | Field { schema; requirement; rest; _ } ->
          let _, schema = extract_field_schema schema in
          let f
              (type field req)
              (schema : field t)
              (requirement : (field, req) requirement)
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
        | FieldEnd -> fun () () -> 0
      in
      compare_fields fields
    | Tuple items ->
      let rec compare_items : type t. t tuple -> t -> t -> int = function
        | ItemEnd -> fun () () -> 0
        | Item { schema; rest } -> (
          fun (l, lr) (r, rr) ->
            match compare schema.descriptor l r with
            | 0 -> compare_items rest lr rr
            | v -> v)
      in
      compare_items items
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

type pack = Pack : _ t -> pack

let pack (type a) (schema : a t) = Pack schema

type 'res unpacker = { unpack : 't. 't t -> 'res }

let unpack { unpack } (Pack schema) = unpack schema

let name_combine schema name =
  match schema.id with
  | Some id -> id ^ "-" ^ name
  | None -> name

let make ?id ?parametric descriptor = { id; descriptor; parametric }

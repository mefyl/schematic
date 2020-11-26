open Base
open Utils
include Schema_intf

let map cons decons { descriptor; _ } =
  { id = None; descriptor = Map (cons, decons, descriptor) }

let rec pp_descriptor : type t kind. (t, kind) descriptor -> _ Pp.t =
  let open Pp in
  let open Pp.O in
  let indent = 2 in
  function
  | Any -> text "any"
  | Bool -> text "bool"
  | Date -> text "date"
  | Int -> text "int"
  | List schema ->
    box ~indent
    @@ text "[" ++ space
       ++ (box @@ pp_schema schema)
       ++ break ~nspaces:1 ~shift:(-indent)
       ++ text "]"
  | Map (_, _, d) -> pp_descriptor d
  | Mapping schema ->
    box ~indent
    @@ text "[" ++ space
       ++ (box @@ pp_schema schema)
       ++ break ~nspaces:1 ~shift:(-indent)
       ++ text "]"
  | Null -> text "()"
  | String -> text "string"
  | String_const v -> escape v
  | Time -> text "time"
  | Time_local -> text "local-time"
  | Object { field; schema; _ } ->
    (* FIXME: other fields *)
    let schema =
      match schema with
      | Inline schema -> schema
      | Outline schema -> schema
    in
    hvbox ~indent
    @@ text "{" ++ space
       ++ box (escape field ++ space ++ char ':' ++ space ++ pp_schema schema)
       ++ break ~nspaces:1 ~shift:(-indent)
       ++ text "}"
  | Tuple _ -> text "FIXME TUPLE"
  | Union _ -> text "FIXME UNION"

and pp_schema : type t. t schema -> _ Pp.t = function
  | { descriptor; _ } -> pp_descriptor descriptor

let pp fmt schema =
  let pp = pp_schema schema in
  Caml.Format.fprintf fmt "%a@." Pp.to_fmt pp

let extract_field_schema = function
  | Inline schema -> (true, schema)
  | Outline schema -> (false, schema)

let assoc_compare compare l r =
  let f (key, l) =
    match List.Assoc.find ~equal:String.equal r key with
    | None -> Some 1
    | Some r -> (
      match compare l r with
      | 0 -> None
      | v -> Some v )
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
    | Int -> Base.Poly.compare
    | List schema -> Base.List.compare (compare schema.descriptor)
    | Map (_, map, v) ->
      let f
          (type u) (f : t -> u) (d : (u, [ `Value ]) descriptor) (l : t) (r : t)
          =
        compare d (f l) (f r)
      in
      f map v
    | Mapping schema -> assoc_compare (compare schema.descriptor)
    | Null -> fun () () -> 0
    | String -> String.compare
    | String_const _ -> fun () () -> 0
    | Time -> Ptime.compare
    | Time_local -> (
      fun (l, lz) (r, rz) ->
        match Int.compare lz rz with
        | 0 -> Ptime.compare l r
        | v -> v )
    | Object { schema; requirement; rest; _ } ->
      let f
          (type field req)
          (schema : field field_schema)
          (req : (field, req) field_requirement)
          ((l : req), lr)
          ((r : req), rr) =
        let _, schema = extract_field_schema schema in
        let c =
          match req with
          | Required -> compare schema.descriptor l r
          | Default _ -> compare schema.descriptor l r
          | Optional -> Option.compare (compare schema.descriptor) l r
        in
        match c with
        | 0 -> compare rest lr rr
        | v -> v
      in
      f schema requirement
    | Tuple { schema; rest } ->
      let f (type field) (schema : field schema) (l, lr) (r, rr) =
        match compare schema.descriptor l r with
        | 0 -> compare rest lr rr
        | v -> v
      in
      f schema
    | Union cases ->
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

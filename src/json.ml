open Batteries
open Utils
include Json_intf

type t = Ezjsonm.value

open Schema

let type_name = function
  | `O _ -> "object"
  | `A _ -> "array"
  | `Bool _ -> "bool"
  | `Float _ -> "float"
  | `String _ -> "string"
  | `Null -> "null"

module List_monad (Monad : Monad) = struct
  let ( >>= ) = Monad.bind

  let rec map f = function
    | [] -> Monad.return []
    | hd :: tl ->
      f hd >>= fun hd ->
      map f tl >>= fun tl -> Monad.return (hd :: tl)
end

module Option_monad (Monad : Monad) = struct
  let ( >>= ) = Monad.bind

  let map f = function
    | None -> Monad.return None
    | Some value -> f value >>= fun value -> Monad.return (Some value)
end

let decode_field :
    type t r. string -> (t, r) field_requirement -> t option -> r Result.t =
 fun field -> function
  | Required -> (
    function
    | Some v -> Result.ok v
    | None ->
      Result.error
        (`Json_decode (Format.sprintf "missing required field %s" field)) )
  | Default default -> (
    function
    | Some v -> Result.ok v
    | None -> Result.ok default )
  | Optional -> Result.ok

let decode schema =
  let rec decode : type t. t descriptor -> Ezjsonm.value -> t Result.t =
   fun schema json ->
    let module List = List_monad (Result) in
    let module Option = Option_monad (Result) in
    let module Syntax = Let_syntax (Result) in
    let open Syntax in
    let type_error expected =
      Result.error
        (`Json_decode
          (Format.sprintf "expected %s, got %s" expected (type_name json)))
    in
    match (schema, json) with
    | Any, v -> Result.ok v
    | Bool, `Bool b -> Result.ok b
    | Bool, _ -> type_error "boolean"
    | End, `O _ -> Result.ok ()
    | End, `A _ -> Result.ok ()
    | End, _ -> failwith "end not on an object or tuple"
    | Int, `Float f -> Result.ok @@ Float.to_int f
    | Int, _ -> type_error "float"
    | List items_schema, `A items ->
      List.map (decode items_schema.descriptor) items
    | List _, _ -> type_error "list"
    | Map (cons, _, schema), _ ->
      decode schema json >>= fun value -> Result.ok @@ cons value
    | Mapping fields_schema, `O fields ->
      let f (name, value) =
        decode fields_schema.descriptor value >>= fun value ->
        Result.ok (name, value)
      in
      List.map f fields
    | Mapping _, _ -> type_error "mapping"
    | Null, `Null -> Result.ok ()
    | Null, _ -> type_error "null"
    | Object { field; requirement; schema; rest; inline; _ }, (`O fields as o)
      ->
      let value =
        if inline then
          Option.map (decode schema.descriptor) (Some o)
        else
          Option.map (decode schema.descriptor)
          @@ Batteries.List.assoc_opt field fields
      in
      value >>= fun value ->
      Result.both (decode_field field requirement value) (decode rest json)
    | Object _, _ -> type_error "object"
    | String, `String s -> Result.ok s
    | String, _ -> type_error "string"
    | String_const expected, `String effective when effective = expected ->
      Result.ok ()
    | String_const expected, `String effective ->
      Result.error
        (`Json_decode
          (Format.sprintf "expected \"%s\", got \"%s\""
             (String.escaped expected) (String.escaped effective)))
    | String_const _, _ -> type_error "string"
    | Tuple { schema; rest = Null }, `A [ hd ] ->
      decode schema.descriptor hd >>= fun hd -> Result.ok (hd, ())
    | Tuple { schema = _; rest = Null }, `A [] ->
      Result.error (`Json_decode "not enough tuple elements")
    | Tuple { schema = _; rest = Null }, `A _ ->
      Result.error (`Json_decode "too many tuple elements")
    | Tuple { schema; rest }, `A (hd :: tl) ->
      decode schema.descriptor hd >>= fun hd ->
      decode rest (`A tl) >>= fun tl -> Result.ok (hd, tl)
    | Tuple _, _ -> type_error "tuple"
    | Union cases, _ -> (
      let conv (Case { schema; decoder; _ }) =
        match decode schema.descriptor json with
        | Batteries.Result.Ok v -> Some (decoder v)
        | Batteries.Result.Error _ -> None
      in
      match Batteries.List.find_map conv cases with
      | res -> Result.ok res
      | exception Not_found ->
        Result.error (`Json_decode "no union case was matched") )
  in
  decode schema.descriptor

let encode_field : type t r. (t, r) field_requirement -> r -> t option =
  function
  | Required -> Option.some
  | Default default -> (
    function
    | v when v = default -> None
    | v -> Some v )
  | Optional -> Batteries.identity

let encode schema =
  let rec encode : type t. t descriptor -> t -> Ezjsonm.value =
   fun schema v ->
    match schema with
    | Any -> v
    | Bool -> `Bool v
    | End -> `Null
    | Int -> `Float (Float.of_int v)
    | List items_schema -> `A (List.map (encode items_schema.descriptor) v)
    | Map (_, decons, schema) -> encode schema @@ decons v
    | Null -> `Null
    | Mapping fields_schema ->
      `O
        (List.map
           (fun (name, v) -> (name, encode fields_schema.descriptor v))
           v)
    | Object { field; requirement; schema; rest; inline; _ } -> (
      let hd, tl = v in
      let rest =
        match encode rest tl with
        | `Null -> []
        | `O rest -> rest
        | _ -> failwith "object schema chained with a non-object schema"
      in
      match encode_field requirement hd with
      | Some hd ->
        let encoded = encode schema.descriptor hd in
        if inline then
          match encoded with
          | `O fields -> `O (List.concat [ fields; rest ])
          | _ -> `O ((field, encoded) :: rest)
        else
          `O ((field, encoded) :: rest)
      | None -> `O rest )
    | Tuple { schema; rest } ->
      let hd, tl = v in
      let rest =
        match encode rest tl with
        | `Null -> []
        | `A rest -> rest
        | _ -> failwith "tuple schema chained with a non-tuple schema"
      in
      `A (encode schema.descriptor hd :: rest)
    | String -> `String v
    | String_const v -> `String v
    | Union cases -> (
      let conv (Case { schema; encoder; _ }) =
        match encoder v with
        | Some v -> Some (encode schema.descriptor v)
        | None -> None
      in
      match List.find_map conv cases with
      | res -> res
      | exception Not_found -> failwith "unmatched variant schema case" )
  in
  encode schema.descriptor

let from_channel = Ezjsonm.value_from_channel

let of_string = Ezjsonm.value_from_string

let to_channel chan t = Ezjsonm.value_to_channel ~minify:true chan t

let to_string t = Ezjsonm.value_to_string ~minify:true t

let rec pp =
  let open Pp in
  let open Pp.O in
  let indent = 2 in
  function
  | `Bool b -> textf "%b" b
  | `Float f -> textf "%f" f
  | `String s -> escape s
  | `Null -> text "null"
  | `A elements ->
    hvbox ~indent
    @@ text "[" ++ space
       ++ concat ~sep:(text "," ++ space) (List.map (box % pp) elements)
       ++ break ~nspaces:1 ~shift:(-indent)
       ++ text "]"
  | `O elements ->
    hvbox ~indent
    @@
    let pp_field (name, value) =
      box (escape name ++ space ++ char ':' ++ space ++ pp value)
    in
    text "{" ++ space
    ++ hvbox (concat ~sep:(text "," ++ space) (List.map pp_field elements))
    ++ break ~nspaces:1 ~shift:(-indent)
    ++ text "}"

let pp fmt json =
  let pp = pp json in
  Format.fprintf fmt "%a@." Pp.to_fmt pp

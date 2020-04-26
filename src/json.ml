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
  let ( let* ) = Monad.bind

  let rec map f = function
    | [] -> Monad.return []
    | hd :: tl ->
      let* hd = f hd in
      let* tl = map f tl in
      Monad.return (hd :: tl)
end

module Option_monad (Monad : Monad) = struct
  let ( let* ) = Monad.bind

  let map f = function
    | None -> Monad.return None
    | Some value ->
      let* value = f value in
      Monad.return (Some value)
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

let rec decode : type t. t schema -> Ezjsonm.value -> t Result.t =
 fun schema json ->
  let module List = List_monad (Result) in
  let module Option = Option_monad (Result) in
  let open Let_syntax (Result) in
  let type_error expected =
    Result.error
      (`Json_decode
        (Format.sprintf "expected %s, got %s" expected (type_name json)))
  in
  match (schema, json) with
  | Null, `Null -> Result.ok ()
  | Null, _ -> type_error "null"
  | Int, `Float f -> Result.ok @@ Float.to_int f
  | Int, _ -> type_error "float"
  | String, `String s -> Result.ok s
  | String, _ -> type_error "string"
  | Bool, `Bool b -> Result.ok b
  | Bool, _ -> type_error "boolean"
  | List items_schema, `A items -> List.map (decode items_schema) items
  | List _, _ -> type_error "list"
  | Mapping fields_schema, `O fields ->
    let f (name, value) =
      let* value = decode fields_schema value in
      Result.ok (name, value)
    in
    List.map f fields
  | Mapping _, _ -> type_error "mapping"
  | Object { field; requirement; schema; rest = Null; _ }, `O fields ->
    let* value =
      Option.map (decode schema) @@ Batteries.List.assoc_opt field fields
    in
    Result.both (decode_field field requirement value) (Result.ok ())
  | Object { field; requirement; schema; rest; _ }, `O fields ->
    let* value =
      Option.map (decode schema) @@ Batteries.List.assoc_opt field fields
    in
    Result.both (decode_field field requirement value) (decode rest json)
  | Object _, _ -> type_error "object"
  | Tuple { schema; rest = Null }, `A [ hd ] ->
    let* hd = decode schema hd in
    Result.ok (hd, ())
  | Tuple { schema = _; rest = Null }, `A [] ->
    Result.error (`Json_decode "not enough tuple elements")
  | Tuple { schema = _; rest = Null }, `A _ ->
    Result.error (`Json_decode "too many tuple elements")
  | Tuple { schema; rest }, `A (hd :: tl) ->
    let* hd = decode schema hd in
    let* tl = decode rest (`A tl) in
    Result.ok (hd, tl)
  | Tuple _, _ -> type_error "tuple"
  | Map (cons, _, schema), _ ->
    let* value = decode schema json in
    Result.ok @@ cons value
  | Union cases, _ -> (
    let conv (Case { schema; decoder; _ }) =
      match decode schema json with
      | Batteries.Result.Ok v -> Some (decoder v)
      | Batteries.Result.Error _ -> None
    in
    match Batteries.List.find_map_opt conv cases with
    | Some res -> Result.ok res
    | None -> Result.error (`Json_decode "no union case was matched") )

let encode_field : type t r. (t, r) field_requirement -> r -> t option =
  function
  | Required -> Option.some
  | Default default -> (
    function
    | v when v = default -> None
    | v -> Some v )
  | Optional -> Batteries.identity

let rec encode : type t. t schema -> t -> Ezjsonm.value =
 fun schema v ->
  match schema with
  | Int -> `Float (Float.of_int v)
  | Null -> `Null
  | String -> `String v
  | Bool -> `Bool v
  | List items_schema -> `A (List.map (encode items_schema) v)
  | Mapping fields_schema ->
    `O (List.map (fun (name, v) -> (name, encode fields_schema v)) v)
  | Object { field; requirement; schema; rest; _ } -> (
    let hd, tl = v in
    let rest =
      match encode rest tl with
      | `Null -> []
      | `O rest -> rest
      | _ -> failwith "object schema chained with a non-object schema"
    in
    match encode_field requirement hd with
    | Some hd -> `O ((field, encode schema hd) :: rest)
    | None -> `O rest )
  | Tuple { schema; rest } ->
    let hd, tl = v in
    let rest =
      match encode rest tl with
      | `Null -> []
      | `A rest -> rest
      | _ -> failwith "tuple schema chained with a non-tuple schema"
    in
    `A (encode schema hd :: rest)
  | Union cases -> (
    let conv (Case { schema; encoder; _ }) =
      match encoder v with
      | Some v -> Some (encode schema v)
      | None -> None
    in
    match List.find_map_opt conv cases with
    | Some res -> res
    | None -> failwith "unmatched variant schema case" )
  | Map (_, decons, schema) -> encode schema @@ decons v

let from_channel = Ezjsonm.value_from_channel

let of_string = Ezjsonm.value_from_string

let to_channel chan t = Ezjsonm.value_to_channel ~minify:true chan t

let to_string t = Ezjsonm.value_to_string ~minify:true t

let rec pp fmt : t -> unit =
  let iter_tail head tail = function
    | [] -> ()
    | [ hd ] -> head hd
    | hd :: tl ->
      head hd;
      List.iter tail tl
  in
  let open Format in
  let indent = 2 in
  function
  | `Bool b -> fprintf fmt "%b" b
  | `Float f -> fprintf fmt "%f" f
  | `String s -> fprintf fmt "\"%s\"" s (* FIXME: escape *)
  | `Null -> fprintf fmt "null"
  | `A elements ->
    let f e =
      pp_print_string fmt ",";
      pp_print_space fmt ();
      pp fmt e
    in
    pp_print_string fmt "[";
    pp_open_hvbox fmt indent;
    iter_tail (pp fmt) f elements;
    pp_close_box fmt ();
    pp_print_string fmt "]"
  | `O elements ->
    let pp_field (name, value) =
      fprintf fmt "\"%s\":" name;
      (* FIXME: escape *)
      pp_print_space fmt ();
      pp fmt value
    in
    let f field =
      pp_print_string fmt ",";
      pp_print_space fmt ();
      pp_field field
    in
    pp_print_string fmt "{";
    pp_open_hvbox fmt indent;
    iter_tail pp_field f elements;
    pp_close_box fmt ();
    pp_print_string fmt "}"

open Base
open Poly
open Utils
include Json_intf

type t = Ezjsonm.value

open Schema

(* FIXME: Base complains that Caml.format is deprecated, but does not provide
   Base.Printf.fprintf *)
let fprintf = Caml.Format.fprintf

let type_name = function
  | `O _ -> "object"
  | `A _ -> "array"
  | `Bool _ -> "bool"
  | `Float _ -> "float"
  | `String _ -> "string"
  | `Null -> "null"

module List_monad (Monad : Monad.S2) = struct
  include List
  open Monad

  let rec map f = function
    | [] -> Monad.return []
    | hd :: tl ->
      f hd >>= fun hd ->
      map f tl >>= fun tl -> Monad.return (hd :: tl)
end

module Option_monad (Monad : Monad.S2) = struct
  include Option
  open Monad

  let map f = function
    | None -> Monad.return None
    | Some value -> f value >>= fun value -> Monad.return (Some value)
end

let decode_field :
    type t r.
    string -> (t, r) field_requirement -> t option -> (r, 'error) Result.t =
 fun field -> function
  | Required -> (
    function
    | Some v -> Result.return v
    | None ->
      Result.fail
        (`Json_decode (Printf.sprintf "missing required field %s" field)) )
  | Default default -> (
    function
    | Some v -> Result.return v
    | None -> Result.return default )
  | Optional -> Result.return

let decode schema =
  let rec decode : type t. t descriptor -> Ezjsonm.value -> (t, 'error) Result.t
      =
   fun schema json ->
    let module List = List_monad (Result) in
    let module Option = Option_monad (Result) in
    let type_error expected =
      Result.fail
        (`Json_decode
          (Printf.sprintf "expected %s, got %s" expected (type_name json)))
    in
    let open Result in
    match (schema, json) with
    | Any, v -> Result.return v
    | Bool, `Bool b -> Result.return b
    | Bool, _ -> type_error "boolean"
    | End, `O _ -> Result.return ()
    | End, `A _ -> Result.return ()
    | End, _ -> failwith "end not on an object or tuple"
    | Int, `Float f -> Result.return @@ Float.to_int f
    | Int, _ -> type_error "float"
    | List items_schema, `A items ->
      List.map (decode items_schema.descriptor) items
    | List _, _ -> type_error "list"
    | Map (cons, _, schema), _ ->
      decode schema json >>= fun value -> Result.return @@ cons value
    | Mapping fields_schema, `O fields ->
      let f (name, value) =
        decode fields_schema.descriptor value >>= fun value ->
        Result.return (name, value)
      in
      List.map f fields
    | Mapping _, _ -> type_error "mapping"
    | Null, `Null -> Result.return ()
    | Null, _ -> type_error "null"
    | Object { field; requirement; schema; rest; inline; _ }, (`O fields as o)
      ->
      let value =
        if inline then
          Option.map (decode schema.descriptor) (Some o)
        else
          Option.map (decode schema.descriptor)
          @@ List.Assoc.find ~equal:( = ) fields field
      in
      value >>= fun value ->
      Result.Let_syntax.Let_syntax.both
        (decode_field field requirement value)
        (decode rest json)
    | Object _, _ -> type_error "object"
    | String, `String s -> Result.return s
    | String, _ -> type_error "string"
    | String_const expected, `String effective when effective = expected ->
      Result.return ()
    | String_const expected, `String effective ->
      Result.fail
        (`Json_decode
          (Printf.sprintf "expected \"%s\", got \"%s\""
             (String.escaped expected) (String.escaped effective)))
    | String_const _, _ -> type_error "string"
    | Tuple { schema; rest = Null }, `A [ hd ] ->
      decode schema.descriptor hd >>= fun hd -> Result.return (hd, ())
    | Tuple { schema = _; rest = Null }, `A [] ->
      Result.fail (`Json_decode "not enough tuple elements")
    | Tuple { schema = _; rest = Null }, `A _ ->
      Result.fail (`Json_decode "too many tuple elements")
    | Tuple { schema; rest }, `A (hd :: tl) ->
      decode schema.descriptor hd >>= fun hd ->
      decode rest (`A tl) >>= fun tl -> Result.return (hd, tl)
    | Tuple _, _ -> type_error "tuple"
    | Union cases, _ -> (
      let conv (Case { schema; decoder; _ }) =
        match decode schema.descriptor json with
        | Result.Ok v -> Some (decoder v)
        | Result.Error _ -> None
      in
      match List.find_map ~f:conv cases with
      | Some res -> Result.return res
      | None -> Result.fail (`Json_decode "no union case was matched") )
  in
  decode schema.descriptor

let encode_field : type t r. (t, r) field_requirement -> r -> t option =
  function
  | Required -> Option.some
  | Default default -> (
    function
    | v when v = default -> None
    | v -> Some v )
  | Optional -> Fn.id

let encode schema =
  let rec encode : type t. t descriptor -> t -> Ezjsonm.value =
   fun schema v ->
    match schema with
    | Any -> v
    | Bool -> `Bool v
    | End -> `Null
    | Int -> `Float (Float.of_int v)
    | List items_schema -> `A (List.map ~f:(encode items_schema.descriptor) v)
    | Map (_, decons, schema) -> encode schema @@ decons v
    | Null -> `Null
    | Mapping fields_schema ->
      `O
        (List.map
           ~f:(fun (name, v) -> (name, encode fields_schema.descriptor v))
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
      match List.find_map ~f:conv cases with
      | Some res -> res
      | None -> failwith "unmatched variant schema case" )
  in
  encode schema.descriptor

let from_channel c =
  match Ezjsonm.value_from_channel c with
  | v -> Result.return v
  | exception Ezjsonm.Parse_error (_, msg) -> Result.fail @@ `Parse msg

let of_string s =
  match Ezjsonm.value_from_string s with
  | v -> Result.return v
  | exception Ezjsonm.Parse_error (_, msg) -> Result.fail @@ `Parse msg

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
       ++ concat
            ~sep:(text "," ++ space)
            (List.map ~f:(Fn.compose box pp) elements)
       ++ break ~nspaces:1 ~shift:(-indent)
       ++ text "]"
  | `O elements ->
    hvbox ~indent
    @@
    let pp_field (name, value) =
      box (escape name ++ space ++ char ':' ++ space ++ pp value)
    in
    text "{" ++ space
    ++ hvbox (concat ~sep:(text "," ++ space) (List.map ~f:pp_field elements))
    ++ break ~nspaces:1 ~shift:(-indent)
    ++ text "}"

let pp fmt json =
  let pp = pp json in
  fprintf fmt "%a@." Pp.to_fmt pp

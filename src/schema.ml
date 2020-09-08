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
  Format.fprintf fmt "%a@." Pp.to_fmt pp

open Base
open Fmt

let composite open_ pp close =
  box ~indent:0
  @@ (hvbox ~indent:2 (const char open_ ++ cut ++ pp) ++ cut ++ const char close)

let colon fmt x = (const char ':' ++ sp) fmt x

let rec of_schema : type t. t Schema.t -> t Fmt.t = function
  | { descriptor; _ } -> of_descriptor descriptor

and of_descriptor : type t. t Schema.descriptor -> t Fmt.t =
  let open Fmt in
  function
  | Any -> fun f json -> Json.pp f json
  | Bool -> bool
  | Date -> fun f (year, month, day) -> pf f "%04d-%02d-%02d" year month day
  | Fixpoint d -> of_descriptor (Lazy.force d)
  | Float -> float
  | Ignore -> nop
  | Int -> int
  | Int64 -> int64
  | List schema -> composite '[' (list ~sep:semi @@ of_schema schema) ']'
  | Map { descriptor; encode; _ } -> using encode @@ of_descriptor descriptor
  | Mapping schema ->
    composite '{'
      (list ~sep:semi @@ pair ~sep:colon Dump.string (of_schema schema))
      '}'
  | Null -> const string "()"
  | Nullable schema -> option ~none:(const string "None") (of_schema schema)
  | Object fields ->
    let pp =
      let f (type req t) (field : (t, req) Schema.field) =
        let pp : req Fmt.t =
          let pp = Schema.field_schema field.schema |> of_schema in
          match field.requirement with
          | Default _ -> pp
          | Required -> pp
          | Optional -> option pp
        in
        const string field.name ++ colon ++ pp ++ semi
      in
      Schema.inline_v { f } fields |> Result.ok_or_failwith
    in
    composite '{' (fun fmt v -> ignore (pp fmt v)) '}'
  | String -> Dump.string
  | String_const s -> const Dump.string s
  | Time -> Ptime.pp
  | Time_local -> fun f (t, tz_offset_s) -> Ptime.pp_human ~tz_offset_s () f t
  | Tuple tuple -> composite '(' (of_tuple tuple) ')'
  | Union { cases; _ } -> of_cases cases

and of_tuple : type t. t Schema.tuple -> t Fmt.t = function
  | ItemEnd -> nop
  | Item { schema; rest = ItemEnd } -> pair ~sep:nop (of_schema schema) nop
  | Item { schema; rest } -> pair ~sep:comma (of_schema schema) (of_tuple rest)

and of_cases : type t. t Schema.case list -> t Fmt.t =
 fun cases fmt v ->
  let cases =
    let f (Schema.Case { schema; encoder; _ }) =
      let pp = of_schema schema in
      fun v -> match encoder v with None -> None | Some v -> Some (pp fmt v)
    in
    List.map ~f cases
  in
  match List.find_map cases ~f:(fun f -> f v) with Some () -> () | None -> ()

open Base
open Fmt

let composite open_ pp close =
  box ~indent:0
  @@ (hvbox ~indent:2 (const char open_ ++ cut ++ pp) ++ cut ++ const char close)

let composite_object open_ pp close =
  box
  @@ const char open_
     ++ (hvbox ~indent:1 @@ (cut ++ pp))
     ++ cut ++ const char close

let colon fmt x = (const char ':' ++ sp) fmt x

let rec of_schema : type t. t Schema.t -> t Fmt.t = function
  | { descriptor; _ } -> of_descriptor descriptor

and of_descriptor : type t. t Schema.descriptor -> t Fmt.t =
  let open Fmt in
  function
  | Any -> fun f json -> Json.pp f json
  | Bool -> bool
  | Date -> fun f (year, month, day) -> pf f "%04d-%02d-%02d" year month day
  | Fixpoint d ->
    (* Break infinite recursion. Would be better to have a fixpoint here too. *)
    fun fmt v -> of_descriptor (Lazy.force d) fmt v
  | Float -> float
  | Ignore -> nop
  | Int -> int
  | Int64 -> int64
  | List schema -> composite_object '[' (list ~sep:semi @@ of_schema schema) ']'
  | Map { descriptor; encode; _ } -> using encode @@ of_descriptor descriptor
  | Mapping schema ->
    composite_object '{'
      (list ~sep:semi @@ pair ~sep:colon Dump.string (of_schema schema))
      '}'
  | Null -> const string "()"
  | Nullable schema -> option ~none:(const string "None") (of_schema schema)
  | Object { fields; encode; _ } ->
    let pp =
      let f (type req t) (field : (t, req) Schema.field) =
        let pp_value : req Fmt.t =
          let pp = Schema.field_schema field.schema |> of_schema in
          match field.requirement with
          | Computed -> option pp
          | Computed_optional -> option (option pp)
          | Default _ -> pp
          | Required -> pp
          | Optional -> option pp
        in
        fun (first, fmt) v ->
          let pp =
            (if first then nop else semi)
            ++ (box ~indent:2
               @@ (const string field.name ++ const string ":" ++ sp ++ pp_value)
               )
          in
          let () = pp fmt v in
          ((false, fmt), ())
      in
      Schema.inline_v_fold { f } fields |> Result.ok_or_failwith
    in
    fun fmt v ->
      let printer =
        composite_object '{'
          (fun fmt v ->
            ignore (pp (true, fmt) v);
            char fmt ';')
          '}'
      in
      printer fmt (encode v)
  | String -> Dump.string
  | String_const s -> const Dump.string s
  | Time -> Ptime.pp
  | Time_local -> fun f (t, tz_offset_s) -> Ptime.pp_human ~tz_offset_s () f t
  | Tuple { items; encode = t_encode; _ } ->
    let printer = composite '(' (of_tuple items) ')' in
    fun fmt v -> printer fmt (t_encode v)
  | Union { cases; _ } -> of_cases cases

and of_tuple : type t. t Schema.tuple -> t Fmt.t = function
  | ItemEnd -> nop
  | Item { schema; rest = ItemEnd } -> pair ~sep:nop (of_schema schema) nop
  | Item { schema; rest } -> pair ~sep:comma (of_schema schema) (of_tuple rest)

and of_cases : type t. t Schema.case list -> t Fmt.t =
 fun cases ->
  let cases =
    let f (Schema.Case { schema; encoder; _ }) =
      let pp = of_schema schema in
      fun fmt v ->
        match encoder v with None -> None | Some v -> Some (pp fmt v)
    in
    List.map ~f cases
  in
  fun fmt v ->
    match List.find_map cases ~f:(fun f -> f fmt v) with
    | Some () -> ()
    | None -> ()

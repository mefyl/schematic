open Utils
include Schema_intf

let required : type a b. (a, b) requirement -> bool = function
  | Required -> true
  | _ -> false

let map cons decons { descriptor; _ } =
  { id = None; descriptor = Map (cons, decons, descriptor); parametric = None }

let extract_field_schema = function
  | Inline schema -> schema
  | Inline_try schema -> schema
  | Outline schema -> schema

(* Pretty printer *)

let rec pp_descriptor : type u. u descriptor Fmt.t =
 fun f d ->
  let fmt =
    let open Fmt in
    let indent = 2 in
    match d with
    | Any -> const string "Any"
    | Bool -> const string "Bool"
    | Date -> const string "Date"
    | Fixpoint s -> const pp_descriptor (Lazy.force s)
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
    let schema = extract_field_schema schema in
    Fmt.(
      const string name ++ const string ": "
      ++ const pp_descriptor schema.descriptor
      ++ comma ++ const pp_fields rest)
      fmt ()

and pp : type u. u t Fmt.t =
 fun f { descriptor; _ } -> pp_descriptor f descriptor

(* Inline helpers *)

type 'res inline = { f : 't. 't field -> 'res }

module Default_walk (Default : WALK_DEFAULT) :
  WALK with type t = Default.t and type fields = Default.fields = struct
  include Default

  let schema _ _ _ = t

  let parametric _ _ _ _ = t

  let fields_empty _ = fields

  let field_inline _ _ f = f

  let field_outline _ _ _ f = f

  let object_ _ = t

  let case _ _ _ = t

  let union _ = t

  let any _ = t

  let bool _ = t

  let float _ = t

  let date _ = t

  let mapping _ = t

  let int _ = t

  let ignore _ = t

  let int64 _ = t

  let list _ = t

  let null _ = t

  let nullable t = t

  let string _ = t

  let string_const _ = t

  let time _ = t

  let time_local _ = t

  let tuple _ = t
end

let rec inline :
    type t res. t descriptor -> res inline -> (res list, string) Result.t =
 fun d f ->
  match d with
  | Ignore -> Result.return [ f.f FieldEnd ]
  | Fixpoint d -> inline (Lazy.force d) f
  | Map (_, _, d) -> inline d f
  | Nullable d -> inline d.descriptor f
  | Object o -> Result.return [ f.f o ]
  | Union { cases; _ } ->
    List.map ~f:(fun (Case { schema; _ }) -> inline schema.descriptor f) cases
    |> Result.all |> Result.map ~f:List.concat
  | s ->
    Result.fail (Fmt.str "inline field is not an object: %a" pp_descriptor s)

type 'res inline_v = { f : 't. 't field -> 't -> 'res }

let rec inline_v :
    type t. t descriptor -> t -> 'res inline_v -> ('res, string) Result.t =
 fun d v f ->
  match d with
  | Ignore -> Result.return (f.f FieldEnd v)
  | Fixpoint d -> inline_v (Lazy.force d) v f
  | Map (_, e, d) -> inline_v d (e v) f
  | Nullable d -> (
    match v with
    | Some v -> inline_v d.descriptor v f
    | None -> Result.return @@ f.f FieldEnd ())
  | Object o -> Result.return (f.f o v)
  | Union { cases; _ } -> (
    let f (Case { encoder; schema; _ }) =
      match encoder v with
      | None -> None
      | Some v -> Some (inline_v schema.descriptor v f)
    in
    match List.find_map ~f cases with
    | None -> Result.fail "no union cased matched in inline_v"
    | Some v -> v)
  | s ->
    Result.fail
      (Fmt.str "inlined value field is not an object: %a" pp_descriptor s)

(* Walker *)

module Walk (W : WALK) = struct
  let rec descriptor : type t. t descriptor -> W.t = function
    | Any -> W.any ()
    | Bool -> W.bool ()
    | Float -> W.float ()
    | Date -> W.date ()
    | Fixpoint s -> descriptor (Lazy.force s)
    | Int -> W.int ()
    | Ignore -> W.ignore ()
    | Int64 -> W.int64 ()
    | List l -> W.list (schema l)
    | Null -> W.null ()
    | String -> W.string ()
    | Time -> W.time ()
    | Time_local -> W.time_local ()
    | Map (_, _, d) -> descriptor d
    | Mapping s -> W.mapping (schema s)
    | Nullable s -> W.nullable (schema s)
    | String_const s -> W.string_const s
    | Object f -> W.object_ (fields f)
    | Tuple l -> W.tuple @@ tuple l
    | Union { cases; key } -> W.union @@ List.map ~f:(case key) cases

  and case : type t. string option -> t case -> W.t =
   fun key (Case { name; schema = s; _ }) -> W.case key name (schema s)

  and schema : type t. t Schema_intf.t -> W.t =
   fun { id; descriptor = d; parametric = p } ->
    let schema () = descriptor d
    and parametric =
      match p with
      | None -> None
      | Some p ->
        Some
          (fun () ->
            let name, args, eff_args, t = parametric p in
            W.parametric name args eff_args t)
    in
    W.schema id parametric schema

  and parametric : type t. t parametric -> string * string list * W.t list * W.t
      = function
    | Plain { name; descriptor = d } -> (name, [], [], descriptor d)
    | Parametric { name; parameter; next } ->
      let template_name, args, eff_args, t =
        parametric
          (next { parameter with id = Some ("#" ^ name); parametric = None })
      in
      (template_name, name :: args, schema parameter :: eff_args, t)

  and fields : type t. t field -> W.fields = function
    | FieldEnd -> W.fields_empty ()
    | Field
        {
          schema =
            ( Outline { descriptor = Ignore; _ }
            | Inline { descriptor = Ignore; _ }
            | Inline_try { descriptor = Ignore; _ } );
          rest;
          _;
        } ->
      fields rest
    | Field { omit = true; rest; _ } -> fields rest
    | Field { field; requirement; rest; schema = Outline s; _ } ->
      let rest = fields rest
      and schema = schema s in
      W.field_outline field (required requirement) schema rest
    | Field { requirement; rest; schema = Inline { descriptor; _ }; _ } ->
      let fields =
        match inline descriptor { f = (fun f -> fields f) } with
        | Result.Ok v -> v
        | Result.Error e -> failwith e
      and rest = fields rest in
      W.field_inline (required requirement) fields rest
    | Field
        {
          field;
          requirement;
          rest;
          schema = Inline_try ({ descriptor; _ } as s);
          _;
        } -> (
      let rest = fields rest in
      match inline descriptor { f = (fun f -> fields f) } with
      | Result.Ok fields -> W.field_inline (required requirement) fields rest
      | Result.Error _ ->
        W.field_outline field (required requirement) (schema s) rest)

  and tuple : type t. t tuple -> W.t list = function
    | ItemEnd -> []
    | Item { schema = s; rest } -> schema s :: tuple rest

  let parametric p =
    let name, args, eff_args, t = parametric p in
    W.parametric name args eff_args t
end

let object_fields =
  let rec object_fields : type t. t field -> string list list =
    let inline schema rest =
      let open Base.Sequence in
      descriptor_fields schema.descriptor
      |> Result.map ~f:(fun fields ->
             cartesian_product (of_list @@ fields)
               (of_list @@ object_fields rest)
             |> map ~f:(fun (l, r) -> l @ r)
             |> to_list)
    and outline field rest =
      List.map ~f:(List.cons field) @@ object_fields rest
    in
    function
    | FieldEnd -> [ [] ]
    | Field { requirement = Default _ | Optional; rest; _ } ->
      object_fields rest
    | Field { schema = Outline { descriptor = Ignore; _ }; rest; _ } ->
      object_fields rest
    | Field { rest; schema = Inline schema; _ } -> (
      match inline schema rest with
      | Result.Ok v -> v
      | Result.Error e -> failwith e)
    | Field { field; rest; schema = Inline_try schema; _ } -> (
      match inline schema rest with
      | Result.Ok v -> v
      | Result.Error _ -> outline field rest)
    | Field { field; rest; _ } -> outline field rest
  and descriptor_fields :
      type t. t descriptor -> (string list list, string) Result.t =
   fun d -> inline d { f = object_fields } |> Result.map ~f:List.concat
  in
  object_fields

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
    | Fixpoint s -> compare (Lazy.force s)
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
          let schema = extract_field_schema schema in
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

let fixpoint f =
  let rec self = Fixpoint (lazy (f self)) in
  f self

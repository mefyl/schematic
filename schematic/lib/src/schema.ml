open Utils
include Schema_intf

let ( let* ) v f = Caml.Result.bind v f
let ( let+ ) v f = Result.map ~f v
let make ?id ?parametric descriptor = { id; descriptor; parametric }

let required : type a b. (a, b) requirement -> bool = function
  | Computed | Required -> true
  | _ -> false

let map ~decode ~encode { descriptor; _ } =
  {
    id = None;
    descriptor = Map { decode; encode; descriptor };
    parametric = None;
  }

let field_schema = function
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
    | Map { descriptor; _ } -> const pp_descriptor descriptor
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
    | Tuple items ->
      box
      @@ (hvbox ~indent
         @@ (const string "[" ++ sp ++ const (hvbox pp_tuple) items))
         ++ sp ++ const string "]"
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

and pp_fields : type t. t fields Fmt.t =
 fun fmt -> function
  | FieldEnd -> ()
  | Field { field = { name; schema; _ }; rest; _ } ->
    let schema = field_schema schema in
    Fmt.(
      const string name ++ const string ": "
      ++ const pp_descriptor schema.descriptor
      ++ comma ++ const pp_fields rest)
      fmt ()

and pp_tuple : type t. t tuple Fmt.t =
 fun fmt -> function
  | ItemEnd -> ()
  | Item { schema; rest } ->
    Fmt.(const pp_descriptor schema.descriptor ++ comma ++ const pp_tuple rest)
      fmt ()

and pp : type u. u t Fmt.t =
 fun f { descriptor; _ } -> pp_descriptor f descriptor

(* Inline helpers *)

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

type 'res walk_fields = { f : 't. 't fields -> 'res }

let rec walk_fields :
    type t res. t descriptor -> res walk_fields -> (res list, string) Result.t =
 fun d f ->
  match d with
  | Ignore -> Result.return [ f.f FieldEnd ]
  | Fixpoint d -> walk_fields (Lazy.force d) f
  | Map { descriptor; _ } -> walk_fields descriptor f
  | Nullable d -> walk_fields d.descriptor f
  | Object o -> Result.return [ f.f o ]
  | Union { cases; key } ->
    List.map
      ~f:(fun (Case { schema; name; _ }) ->
        let f =
          match key with
          | None -> f
          | Some key ->
            {
              f =
                (fun fields ->
                  f.f
                    (Field
                       {
                         field =
                           {
                             name = key;
                             schema = Outline (make (String_const name));
                             description = None;
                             examples = [];
                             maximum = None;
                             minimum = None;
                             omit = false;
                             requirement = Required;
                             title = None;
                           };
                         rest = fields;
                       }));
            }
        in
        walk_fields schema.descriptor f)
      cases
    |> Result.all |> Result.map ~f:List.concat
  | s ->
    Result.fail (Fmt.str "inline field is not an object: %a" pp_descriptor s)

let field_ignored : type req t. (t, req) field -> bool = function
  | { schema = Outline { descriptor = Ignore; _ }; _ } -> true
  | { schema = Inline_try { descriptor = Ignore; _ }; _ } -> true
  | { omit; _ } -> omit

(* FIXME: Also flatten single-case unions. *)
(* FIXME:? Offer to flatten single field objects, tuples, ... *)
let rec flatten : type t. t descriptor -> t flatten =
  let open Result in
  function
  | Fixpoint descriptor -> Lazy.force descriptor |> flatten
  | Map { descriptor; decode; encode } ->
    let (Flatten { descriptor; decode = d_decode; encode = d_encode }) =
      flatten descriptor
    in
    let decode v = d_decode v >>= decode
    and encode v = encode v |> d_encode in
    Flatten { descriptor; decode; encode }
  | descriptor -> Flatten { descriptor; decode = Result.return; encode = Fn.id }

(* Object inlining *)

let inline desc fold f =
  let rec inline :
      type req res t.
      t descriptor ->
      (t, req) requirement ->
      'fold ->
      ('fold, res) inline ->
      ('fold * res list list, string) Result.t =
   fun d req acc f ->
    match d with
    | Ignore -> Result.return (acc, [ [] ])
    | Fixpoint d -> inline (Lazy.force d) req acc f
    | Map { descriptor; encode; _ } -> (
      match req with
      | Computed -> inline descriptor Computed acc f
      | Computed_optional -> inline descriptor Computed_optional acc f
      | Default d -> inline descriptor (Default (encode d)) acc f
      | Optional -> inline descriptor Optional acc f
      | Required -> inline descriptor Required acc f)
    | Nullable d -> inline d.descriptor Optional acc f
    | Object fields ->
      let rec inline_fields :
          type req t.
          (t, req) requirement ->
          'fold ->
          t fields ->
          ('fold * res list list, string) Result.t =
       fun req acc -> function
        | FieldEnd -> Result.return (acc, [ [] ])
        | Field { field; rest } when field_ignored field -> (
          match req with
          | Computed -> inline_fields Computed acc rest
          | Computed_optional -> inline_fields Computed_optional acc rest
          | Default (_, tail) -> inline_fields (Default tail) acc rest
          | Optional -> inline_fields Optional acc rest
          | Required -> inline_fields Required acc rest)
        | Field { field; rest } ->
          let* acc, head =
            let inline =
              match field.schema with
              | Inline schema | Inline_try schema -> (
                match (field.requirement, req) with
                | (Computed | Computed_optional | Default _ | Optional), _
                | Required, Required ->
                  inline schema.descriptor field.requirement acc f
                | Required, Default (head, _) ->
                  inline schema.descriptor (Default head) acc f
                | Required, ((Computed | Optional) as requirement) ->
                  inline schema.descriptor requirement acc f
                | Required, Computed_optional ->
                  inline schema.descriptor Computed_optional acc f)
              | Outline _ -> Result.fail "unused"
            in
            match (field.schema, inline) with
            | Inline _, _ -> inline
            | Inline_try _, Result.Ok _ -> inline
            | Inline_try _, Result.Error _ | Outline _, _ ->
              let acc, head =
                match (field.requirement, req) with
                | (Computed | Computed_optional | Default _ | Optional), _
                | Required, Required ->
                  f.f acc field
                | Required, ((Computed | Optional) as requirement) ->
                  f.f acc
                    {
                      description = field.description;
                      examples = field.examples;
                      maximum = field.maximum;
                      minimum = field.minimum;
                      name = field.name;
                      omit = field.omit;
                      requirement;
                      schema = field.schema;
                      title = field.title;
                    }
                | Required, (Computed_optional as requirement) ->
                  f.f acc
                    {
                      description = field.description;
                      examples = field.examples;
                      maximum = field.maximum;
                      minimum = field.minimum;
                      name = field.name;
                      omit = field.omit;
                      requirement;
                      schema = field.schema;
                      title = field.title;
                    }
                | Required, Default (head, _) ->
                  f.f acc
                    {
                      description = field.description;
                      examples = field.examples;
                      maximum = field.maximum;
                      minimum = field.minimum;
                      name = field.name;
                      omit = field.omit;
                      requirement = Default head;
                      schema = field.schema;
                      title = field.title;
                    }
              in
              Result.return (acc, [ [ head ] ])
          in
          let+ acc, tail =
            match req with
            | Computed -> inline_fields Computed acc rest
            | Computed_optional -> inline_fields Computed_optional acc rest
            | Default (_, tail) -> inline_fields (Default tail) acc rest
            | Optional -> inline_fields Optional acc rest
            | Required -> inline_fields Required acc rest
          in
          ( acc,
            Sequence.(
              cartesian_product (of_list head) (of_list tail)
              |> map ~f:(fun (h, t) -> h @ t)
              |> to_list) )
      in
      inline_fields req acc fields
    | Union { cases; key } ->
      let f acc (Case { schema; name; _ }) =
        let acc, key =
          match key with
          | None -> (acc, None)
          | Some key ->
            let acc, key =
              f.f acc
                {
                  name = key;
                  schema = Outline (make (String_const name));
                  description = None;
                  examples = [];
                  maximum = None;
                  minimum = None;
                  omit = false;
                  requirement = Required;
                  title = None;
                }
            in
            (acc, Some key)
        in
        match inline schema.descriptor Required acc f with
        | Result.Ok (acc, res) -> (
          match key with
          | Some key ->
            (acc, List.map ~f:(fun l -> key :: l) res |> Result.return)
          | None -> (acc, Result.return res))
        | Result.Error _ as e -> (acc, e)
      in
      let acc, res = List.fold_map ~init:acc ~f cases in
      let+ res = Result.all res in
      (acc, List.concat res)
    | s ->
      Result.fail (Fmt.str "inline field is not an object: %a" pp_descriptor s)
  in
  inline desc Required fold f

let inline_v_fold (f : ('fold, 'res) inline_v_fold) fields =
  let rec inline_field :
      type req t.
      (t, req) field -> ('fold -> req -> 'fold * 'res list, string) Result.t =
   fun field ->
    let inline : ('fold -> req -> 'fold * 'res list, string) Result.t =
      match field.schema with
      | Inline schema | Inline_try schema -> (
        let+ f = inline_descriptor schema.descriptor in
        match field.requirement with
        | Required ->
          (fun fold v -> f fold v : 'fold -> req -> 'fold * 'res list)
        | Default _ -> fun fold v -> f fold v
        | Computed -> (
          fun fold -> function Some v -> f fold v | None -> (fold, []))
        | Computed_optional -> (
          fun fold -> function Some (Some v) -> f fold v | _ -> (fold, []))
        | Optional -> (
          fun fold -> function Some v -> f fold v | None -> (fold, [])))
      | Outline _ -> Result.fail "unused"
    in
    match (field.schema, inline) with
    | Inline _, _ -> inline
    | Inline_try _, Result.Ok _ -> inline
    | Inline_try _, Result.Error _ | Outline _, _ ->
      let f = f.f field in
      Result.return @@ fun fold v ->
      let fold, res = f fold v in
      (fold, [ res ])
  and inline_fields :
      type t. t fields -> ('fold -> t -> 'fold * 'res list, string) Result.t =
    function
    | FieldEnd -> Result.return (fun fold () -> (fold, []))
    | Field { field; rest } ->
      let* tail = inline_fields rest in
      if field_ignored field then
        Result.return (fun fold (_, tl) -> tail fold tl)
      else
        let+ head = inline_field field in
        fun fold (hd, tl) ->
          let fold, head = head fold hd in
          let fold, tail = tail fold tl in
          (fold, head @ tail)
  and inline_descriptor :
      type t. t descriptor -> ('fold -> t -> 'fold * 'res list, string) Result.t
      = function
    | Ignore -> Result.return (fun fold () -> (fold, []))
    | Fixpoint d -> inline_descriptor (Lazy.force d)
    | Map { descriptor; encode; _ } ->
      let+ f = inline_descriptor descriptor in
      fun fold v -> f fold (encode v)
    | Nullable schema -> (
      let+ f = inline_descriptor schema.descriptor in
      fun fold -> function Some v -> f fold v | None -> (fold, []))
    | Object fields -> inline_fields fields
    | Union { cases; key } -> (
      let+ cases =
        let f (Case { encoder; name; schema; _ }) =
          let+ res = inline_descriptor schema.descriptor in
          let key =
            match key with
            | None -> None
            | Some key ->
              Some
                (f.f
                   {
                     name = key;
                     schema = Outline (make (String_const name));
                     description = None;
                     examples = [];
                     maximum = None;
                     minimum = None;
                     omit = false;
                     requirement = Required;
                     title = None;
                   })
          in
          fun fold v ->
            match encoder v with
            | None -> None
            | Some v -> (
              match key with
              | None -> Some (res fold v)
              | Some key ->
                let fold, key = key fold () in
                let fold, res = res fold v in
                Some (fold, key :: res))
        in
        List.map ~f cases |> Result.all
      in
      fun fold v ->
        match List.find_map cases ~f:(fun case -> case fold v) with
        | Some case -> case
        | None -> failwith "no union cased matched in inline_f")
    | s ->
      Result.fail (Fmt.str "inline field is not an object: %a" pp_descriptor s)
  in
  inline_fields fields

let inline_v (f : _ inline_v) fields =
  let inline : (_, _) inline_v_fold =
    { f = (fun field () v -> ((), f.f field v)) }
  in
  match inline_v_fold inline fields with
  | Result.Ok f ->
    Result.return (fun t ->
        let (), res = f () t in
        res)
  | Result.Error _ as error -> error

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
    | Map { descriptor = d; _ } -> descriptor d
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

  and fields : type t. t fields -> W.fields = function
    | FieldEnd -> W.fields_empty ()
    | Field
        {
          field =
            {
              schema =
                ( Outline { descriptor = Ignore; _ }
                | Inline { descriptor = Ignore; _ }
                | Inline_try { descriptor = Ignore; _ } );
              _;
            };
          rest;
        } ->
      fields rest
    | Field { field = { omit = true; _ }; rest } -> fields rest
    | Field { field = { name; requirement; schema = Outline s; _ }; rest } ->
      let rest = fields rest
      and schema = schema s in
      W.field_outline name (required requirement) schema rest
    | Field
        { field = { requirement; schema = Inline { descriptor; _ }; _ }; rest }
      ->
      let fields =
        match walk_fields descriptor { f = (fun f -> fields f) } with
        | Result.Ok v -> v
        | Result.Error e -> failwith e
      and rest = fields rest in
      W.field_inline (required requirement) fields rest
    | Field
        {
          field =
            {
              name;
              requirement;
              schema = Inline_try ({ descriptor; _ } as s);
              _;
            };
          rest;
        } -> (
      let rest = fields rest in
      match walk_fields descriptor { f = (fun f -> fields f) } with
      | Result.Ok fields -> W.field_inline (required requirement) fields rest
      | Result.Error _ ->
        W.field_outline name (required requirement) (schema s) rest)

  and tuple : type t. t tuple -> W.t list = function
    | ItemEnd -> []
    | Item { schema = s; rest } -> schema s :: tuple rest

  let parametric p =
    let name, args, eff_args, t = parametric p in
    W.parametric name args eff_args t
end

let object_fields descriptor =
  let+ (), names =
    inline descriptor ()
      {
        f =
          (fun () { name; requirement; _ } ->
            ((), Option.some_if (required requirement) name));
      }
  in
  List.map ~f:(List.filter_map ~f:Fn.id) names

let assoc_compare compare l r =
  let f (key, l) =
    match List.Assoc.find ~equal:String.equal r key with
    | None -> Some 1
    | Some r -> ( match compare l r with 0 -> None | v -> Some v)
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
    | Int | Int64 -> Base.Poly.compare
    | List schema -> Base.List.compare (compare schema.descriptor)
    | Map { encode; descriptor; _ } ->
      fun l r -> compare descriptor (encode l) (encode r)
    | Mapping schema -> assoc_compare (compare schema.descriptor)
    | Null -> fun () () -> 0
    | Nullable s -> fun l r -> Base.Option.compare (compare s.descriptor) l r
    | String -> String.compare
    | String_const _ -> fun () () -> 0
    | Time -> Ptime.compare
    | Time_local -> (
      fun (l, lz) (r, rz) ->
        match Int.compare lz rz with 0 -> Ptime.compare l r | v -> v)
    | Object fields ->
      let rec compare_fields : type u. u fields -> u -> u -> int = function
        | Field { field = { schema; requirement; _ }; rest } ->
          let schema = field_schema schema in
          let f (type field req) (schema : field t)
              (requirement : (field, req) requirement) ((l : req), lr)
              ((r : req), rr) =
            let c =
              match requirement with
              | Computed -> Option.compare (compare schema.descriptor) l r
              | Computed_optional ->
                Option.compare (Option.compare (compare schema.descriptor)) l r
              | Default _ -> compare schema.descriptor l r
              | Optional -> Option.compare (compare schema.descriptor) l r
              | Required -> compare schema.descriptor l r
            in
            match c with 0 -> compare_fields rest lr rr | v -> v
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
  match schema.id with Some id -> id ^ "-" ^ name | None -> name

let fixpoint f =
  let rec self = Fixpoint (lazy (f self)) in
  f self

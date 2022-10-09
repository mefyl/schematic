open Utils
include Schema_intf

let ( let* ) = Caml.Result.bind
let ( let+ ) v f = Caml.Result.map f v
let ( and+ ) = Base.Result.Let_syntax.Let_syntax.both
let make ?id ?parametric descriptor = { id; descriptor; parametric }

let required : type a b. (a, b) requirement -> bool = function
  | Computed | Required -> true
  | _ -> false

let map ~decode ~encode { descriptor; id; _ } =
  { id; descriptor = Map { decode; encode; descriptor }; parametric = None }

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
    | Object { fields; _ } ->
      box
      @@ (hvbox ~indent
         @@ (const string "{" ++ sp ++ const (hvbox pp_fields) fields))
         ++ sp ++ const string "}"
    | Tuple { items; _ } ->
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

type 'res walk_fields = { f : 't. 't fields -> 'res }

let rec walk_fields :
    type t res. t descriptor -> res walk_fields -> (res list, string) Result.t =
 fun d f ->
  match d with
  | Ignore -> Result.return [ f.f FieldEnd ]
  | Fixpoint d -> walk_fields (Lazy.force d) f
  | Map { descriptor; _ } -> walk_fields descriptor f
  | Nullable d -> walk_fields d.descriptor f
  | Object { fields; _ } -> Result.return [ f.f fields ]
  | Union { cases; key; _ } ->
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
  | { schema = Inline { descriptor = Ignore; _ }; _ } -> true
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

module Make_inline (M : Base.Monad.S) = struct
  include Make_inline (M)

  let ( let++ ) = M.Let_syntax.( >>| )
  let ( and++ ) = M.Let_syntax.Let_syntax.both

  let schema desc f =
    let rec inline :
        type req res t.
        t descriptor ->
        (t, req) requirement ->
        res inline ->
        (res list list M.t, string) Result.t =
     fun d req f ->
      match d with
      | Ignore -> M.return [ [] ] |> Result.return
      | Fixpoint d -> inline (Lazy.force d) req f
      | Map { descriptor; encode; _ } -> (
        match req with
        | Computed -> inline descriptor Computed f
        | Computed_optional -> inline descriptor Computed_optional f
        | Default d -> inline descriptor (Default (encode d)) f
        | Optional -> inline descriptor Optional f
        | Required -> inline descriptor Required f)
      | Nullable d -> inline d.descriptor Optional f
      | Object { fields; encode; _ } -> (
        let rec inline_fields :
            type req f.
            (f, req) requirement ->
            f fields ->
            (res list list M.t, string) Result.t =
         fun req -> function
          | FieldEnd -> M.return [ [] ] |> Result.return
          | Field { field; rest } when field_ignored field -> (
            match req with
            | Computed -> inline_fields Computed rest
            | Computed_optional -> inline_fields Computed_optional rest
            | Default (_, tail) -> inline_fields (Default tail) rest
            | Optional -> inline_fields Optional rest
            | Required -> inline_fields Required rest)
          | Field { field; rest } ->
            let+ head =
              let inline =
                match field.schema with
                | Inline schema | Inline_try schema -> (
                  match (field.requirement, req) with
                  | (Computed | Computed_optional | Default _ | Optional), _
                  | Required, Required ->
                    inline schema.descriptor field.requirement f
                  | Required, Default (head, _) ->
                    inline schema.descriptor (Default head) f
                  | Required, ((Computed | Optional) as requirement) ->
                    inline schema.descriptor requirement f
                  | Required, Computed_optional ->
                    inline schema.descriptor Computed_optional f)
                | Outline _ -> Result.fail "unused"
              in
              match (field.schema, inline) with
              | Inline _, _ -> inline
              | Inline_try _, Result.Ok _ -> inline
              | Inline_try _, Result.Error _ | Outline _, _ ->
                Result.return
                @@ let++ head =
                     match (field.requirement, req) with
                     | (Computed | Computed_optional | Default _ | Optional), _
                     | Required, Required ->
                       f.f field
                     | Required, ((Computed | Optional) as requirement) ->
                       f.f
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
                       f.f
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
                       f.f
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
                   [ [ head ] ]
            and+ tail =
              match req with
              | Computed -> inline_fields Computed rest
              | Computed_optional -> inline_fields Computed_optional rest
              | Default (_, tail) -> inline_fields (Default tail) rest
              | Optional -> inline_fields Optional rest
              | Required -> inline_fields Required rest
            in
            let++ head = head
            and++ tail = tail in
            Sequence.(
              cartesian_product (of_list head) (of_list tail)
              |> map ~f:(fun (h, t) -> h @ t)
              |> to_list)
        in
        match req with
        | Computed -> inline_fields Computed fields
        | Computed_optional -> inline_fields Computed_optional fields
        | Default v -> inline_fields (Default (encode v)) fields
        | Required -> inline_fields Required fields
        | Optional -> inline_fields Optional fields)
      | Union { cases; key; _ } ->
        let f (Case { schema; name; _ }) =
          let key =
            match key with
            | None -> M.return None
            | Some key ->
              f.f
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
              |> M.map ~f:Option.return
          in
          let+ res = inline schema.descriptor Required f in
          let++ key = key
          and++ res = res in
          match key with
          | Some key -> List.map ~f:(fun l -> key :: l) res
          | None -> res
        in
        let res = List.map ~f cases in
        let+ res = Result.all res in
        let++ res = M.all res in
        List.concat res
      | s ->
        Result.fail
          (Fmt.str "inline field is not an object: %a" pp_descriptor s)
    in
    inline desc Required f

  let value (f : 'res inline_v) fields =
    let rec inline_field :
        type req t. (t, req) field -> (req -> 'res list M.t, string) Result.t =
     fun field ->
      let inline : (req -> 'res list M.t, string) Result.t =
        match field.schema with
        | Inline schema | Inline_try schema -> (
          let+ f = inline_descriptor schema.descriptor in
          match field.requirement with
          | Required -> (f : req -> 'res list M.t)
          | Default _ -> f
          | Computed -> ( function Some v -> f v | None -> M.return [])
          | Computed_optional -> (
            function Some (Some v) -> f v | _ -> M.return [])
          | Optional -> ( function Some v -> f v | None -> M.return []))
        | Outline _ -> Result.fail "unused"
      in
      match (field.schema, inline) with
      | Inline _, _ -> inline
      | Inline_try _, Result.Ok _ -> inline
      | Inline_try _, Result.Error _ | Outline _, _ ->
        Result.return
        @@
        let f = f.f field in
        fun v ->
          let++ res = f v in
          [ res ]
    and inline_fields :
        type t. t fields -> (t -> 'res list M.t, string) Result.t = function
      | FieldEnd -> Result.return (fun () -> M.return [])
      | Field { field; rest } ->
        let* tail = inline_fields rest in
        if field_ignored field then Result.return (fun (_, tl) -> tail tl)
        else
          let+ head = inline_field field in
          fun (hd, tl) ->
            let++ head = head hd
            and++ tail = tail tl in
            head @ tail
    and inline_descriptor :
        type t. t descriptor -> (t -> 'res list M.t, string) Result.t = function
      | Ignore -> Result.return (fun () -> M.return [])
      | Fixpoint d ->
        (* Break infinite recursion. Would be better to have a fixpoint here
           too. *)
        Result.return (fun v ->
            (inline_descriptor (Lazy.force d) |> Result.ok_or_failwith) v)
      | Map { descriptor; encode; _ } ->
        let+ f = inline_descriptor descriptor in
        fun v -> f (encode v)
      | Nullable schema -> (
        let+ f = inline_descriptor schema.descriptor in
        function Some v -> f v | None -> M.return [])
      | Object { fields; encode; _ } ->
        let+ f = inline_fields fields in
        fun v -> f (encode v)
      | Union { cases; key; _ } -> (
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
            fun v ->
              match encoder v with
              | None -> None
              | Some v -> (
                match key with
                | None -> Some (res v)
                | Some key ->
                  Option.return
                  @@ let++ key = key ()
                     and++ res = res v in
                     key :: res)
          in
          List.map ~f cases |> Result.all
        in
        fun v ->
          match List.find_map cases ~f:(fun case -> case v) with
          | Some case -> case
          | None -> failwith "no union cased matched in inline_f")
      | s ->
        Result.fail
          (Fmt.str "inline field is not an object: %a" pp_descriptor s)
    in
    inline_fields fields
end

module Inline = Make_inline (Base.Monad.Ident)

(* Walker *)

module Walk (W : WALK) = struct
  let ( let+ ) = W.M.Let_syntax.( >>| )
  let ( and+ ) = W.M.Let_syntax.Let_syntax.both

  let rec descriptor : type t. t descriptor -> W.t W.M.t = function
    | Any -> W.any
    | Bool -> W.bool
    | Float -> W.float
    | Date -> W.date
    | Fixpoint s -> descriptor (Lazy.force s)
    | Int -> W.int
    | Ignore -> W.ignore
    | Int64 -> W.int64
    | List l -> W.list (schema l)
    | Null -> W.null
    | String -> W.string
    | Time -> W.time
    | Time_local -> W.time_local
    | Map { descriptor = d; _ } -> descriptor d
    | Mapping s -> W.mapping (schema s)
    | Nullable s -> W.nullable (schema s)
    | String_const s -> W.string_const s
    | Object { fields = f; _ } -> W.object_ (fields true f)
    | Tuple { items; _ } -> W.tuple @@ tuple items
    | Union { cases; key; _ } ->
      let cases = List.map ~f:(case key) cases |> W.M.all in
      W.union cases

  and case : type t. string option -> t case -> W.t W.M.t =
   fun key (Case { name; schema = s; _ }) -> W.case key name (schema s)

  and schema : type t. t Schema_intf.t -> W.t W.M.t =
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

  and parametric :
      type t. t parametric -> string * string list * W.t W.M.t list * W.t W.M.t
      = function
    | Plain { name; descriptor = d } -> (name, [], [], descriptor d)
    | Parametric { name; parameter; next } ->
      let template_name, args, eff_args, t =
        parametric
          (next { parameter with id = Some ("#" ^ name); parametric = None })
      in
      (template_name, name :: args, schema parameter :: eff_args, t)

  and fields : type t. bool -> t fields -> W.field list list W.M.t =
    let field_outline inline_required name requirement s rest =
      let+ field =
        W.field name (required requirement && inline_required) (schema s)
      and+ rest = fields inline_required rest in
      List.map ~f:(List.cons field) rest
    and field_inline inline_required requirement s rest =
      match
        walk_fields s.descriptor
          { f = (fun f -> fields (required requirement && inline_required) f) }
      with
      | Result.Error _ as e -> e
      | Result.Ok v ->
        Result.return
        @@ let+ fields = W.M.all v
           and+ rest = fields (inline_required && required requirement) rest in
           let fields =
             Base.Sequence.(of_list fields |> map ~f:of_list |> concat)
           in
           Base.Sequence.(
             cartesian_product fields (of_list rest)
             |> map ~f:(fun (l, r) -> l @ r)
             |> to_list)
    in
    fun inline_required -> function
      | FieldEnd -> W.M.return [ [] ]
      | Field { field; rest } when field_ignored field ->
        fields inline_required rest
      | Field { field = { name; requirement; schema = Outline s; _ }; rest } ->
        field_outline inline_required name requirement s rest
      | Field { field = { requirement; schema = Inline schema; _ }; rest } -> (
        match field_inline inline_required requirement schema rest with
        | Result.Ok v -> v
        | Result.Error e -> failwith e)
      | Field
          { field = { name; requirement; schema = Inline_try schema; _ }; rest }
        -> (
        match field_inline inline_required requirement schema rest with
        | Result.Ok v -> v
        | Result.Error _ ->
          field_outline inline_required name requirement schema rest)

  and tuple : type t. t tuple -> W.t list W.M.t = function
    | ItemEnd -> W.M.return []
    | Item { schema = s; rest } ->
      let+ s = schema s
      and+ rest = tuple rest in
      s :: rest

  let parametric p =
    let name, args, eff_args, t = parametric p in
    W.parametric name args eff_args t
end

let object_fields descriptor =
  let+ names =
    Inline.schema descriptor
      {
        f =
          (fun { name; requirement; _ } ->
            Option.some_if (required requirement) name);
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
    | Object { fields; encode; decode = _ } ->
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
      let compare_items = compare_fields fields in
      fun l r -> compare_items (encode l) (encode r)
    | Tuple { items; encode; decode = _ } ->
      let rec compare_items : type t. t tuple -> t -> t -> int = function
        | ItemEnd -> fun () () -> 0
        | Item { schema; rest } -> (
          fun (l, lr) (r, rr) ->
            match compare schema.descriptor l r with
            | 0 -> compare_items rest lr rr
            | v -> v)
      in
      let compare_items = compare_items items in
      fun l r -> compare_items (encode l) (encode r)
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

let equal schema l r = compare schema l r = 0

type pack = Pack : _ t -> pack

let pack (type a) (schema : a t) = Pack schema

type 'res unpacker = { unpack : 't. 't t -> 'res }

let unpack { unpack } (Pack schema) = unpack schema

let name_combine schema name =
  match schema.id with Some id -> id ^ "-" ^ name | None -> name

let fixpoint f =
  let rec self = Fixpoint (lazy (f self)) in
  f self

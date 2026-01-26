open Utils
include Schema_intf

let ( let* ) = Stdlib.Result.bind
let ( let+ ) v f = Stdlib.Result.map f v
let ( and+ ) = Base.Result.Let_syntax.Let_syntax.both
let make ?id ?parametric descriptor = { id; descriptor; parametric }

let computed : type a b. (a, b) requirement -> bool = function
  | Computed | Computed_optional -> true
  | _ -> false

let required : type a b. (a, b) requirement -> bool = function
  | Computed | Required -> true
  | _ -> false

let is_optional : type a b. (a, b) requirement -> bool = function
  | Optional -> true
  | _ -> false

let map ~decode ~encode { descriptor; id; _ } =
  { id; descriptor = Map { decode; encode; descriptor }; parametric = None }

let field_schema = function
  | Inline schema -> schema
  | Inline_try schema -> schema
  | Outline schema -> schema

(* Pretty printer *)

module Marks = Stdlib.Set.Make (String)

let pp_container' ?(delimiters = ("{", "}")) ~indent pp_content =
  let intro, outro = delimiters in
  let open Fmt in
  let pp_lobby f _ =
    Stdlib.Format.pp_print_custom_break ~fits:("", 1, "")
      ~breaks:("", indent, "") f
  in
  hvbox
  @@ const string intro
     ++ pp_lobby
     ++ (hvbox @@ pp_content)
     ++ sp
     ++ const string outro

let pp_container ?delimiters ?(sep = Fmt.comma) ~indent pp_item =
  let open Fmt in
  pp_container' ?delimiters ~indent (seq ~sep (box ~indent pp_item))

let rec pp_descriptor : type u. Marks.t -> u descriptor Fmt.t =
  let indent = 2 in
  let pp_case : type u. Marks.t -> u case Fmt.t =
   fun marks f (Case { name; schema; since; _ }) ->
    let pp =
      let open Fmt in
      const Dump.string name
      ++ const char ':'
      ++ sp
      ++ const
           (pp_container' ~indent
              (using fst
                 (option
                    (const string "since: " ++ using fst Semver.pp ++ comma))
              ++ (using snd
                 @@ box ~indent (const string "schema:" ++ sp ++ pp marks))))
           (since, schema)
    in
    pp f ()
  in
  fun marks f d ->
    let fmt =
      let open Fmt in
      match d with
      | Any _ -> const string "Any"
      | Bool -> const string "Bool"
      | Coding { f } -> const (pp_descriptor marks) (f None)
      | Date -> const string "Date"
      | Fixpoint s -> const (pp_descriptor marks) (Lazy.force s)
      | Float -> const string "Float"
      | Ignore -> const string "Ignore"
      | Int -> const string "Int"
      | Int64 -> const string "Int64"
      | Json -> const string "Json"
      | List schema ->
        box ~indent
        @@ const string "["
           ++ sp
           ++ (box @@ const (pp marks) schema)
           ++ sp
           ++ const string "]"
      | Map { descriptor; _ } -> const (pp_descriptor marks) descriptor
      | Mapping { item; _ } -> const (pp marks) item
      | Null -> const string "Null"
      | Nullable s ->
        const string "Nullable (" ++ const (pp marks) s ++ const string ")"
      | Sequence { item; _ } ->
        box ~indent
        @@ const string "["
           ++ sp
           ++ (box @@ const (pp marks) item)
           ++ sp
           ++ const string "]"
      | String -> const string "String"
      | String_const v -> escape v
      | Time -> const string "Time"
      | Time_local -> const string "Time_local"
      | Object { fields; _ } ->
        let open struct
          type any_field = Field : _ field -> any_field
          type any_fields = Fields : _ fields -> any_fields
        end in
        let pp_field fmt (Field { name; schema; requirement; _ }) =
          let schema = field_schema schema in
          Fmt.pf fmt "%s (%a):@ %a" name pp_requirement requirement (pp marks)
            schema
        and fields =
          let unfold = function
            | Fields FieldEnd -> None
            | Fields (Field { field; rest }) -> Some (Field field, Fields rest)
          in
          Stdlib.Seq.unfold unfold (Fields fields)
        in
        const (pp_container ~indent pp_field) fields
      | Tuple { items; _ } ->
        box
        @@ (hvbox ~indent
           @@ (const string "[" ++ sp ++ const (hvbox (pp_tuple marks)) items))
           ++ sp
           ++ const string "]"
      | Union { cases; _ } ->
        const
          (pp_container ~indent ~delimiters:("Union {", "}") (pp_case marks))
        @@ Stdlib.List.to_seq cases
    in
    fmt f ()

and pp_requirement : type t req. (t, req) requirement Fmt.t =
 fun f -> function
  | Computed -> Fmt.string f "Computed"
  | Computed_optional -> Fmt.string f "Computed_optional"
  | Default _ -> Fmt.string f "Default"
  | Required -> Fmt.string f "Required"
  | Optional -> Fmt.string f "Optional"

and pp_tuple : type t. Marks.t -> t tuple Fmt.t =
 fun marks fmt -> function
  | ItemEnd -> ()
  | Item { schema; rest } ->
    Fmt.(const (pp marks) schema ++ comma ++ const (pp_tuple marks) rest) fmt ()

and pp : type u. Marks.t -> u t Fmt.t =
 fun marks f { descriptor; id; _ } ->
  match id with
  | Some id when Marks.mem id marks -> Fmt.fmt "<%s>" f id
  | Some id ->
    let marks = Marks.add id marks in
    Fmt.fmt "<%s> =@ %a" f id (pp_descriptor marks) descriptor
  | None -> (pp_descriptor marks) f descriptor

let pp schema = pp Marks.empty schema
let pp_descriptor descriptor = pp_descriptor Marks.empty descriptor

(* Inline helpers *)

type 'res walk_fields = { f : 't. 't Hlist.t fields -> 'res }

let rec walk_fields : type t res.
    t descriptor -> res walk_fields -> (res list, string) Result.t =
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
                             since = None;
                             title = None;
                           };
                         rest = fields;
                       }));
            }
        in
        walk_fields schema.descriptor f)
      cases
    |> Result.all
    |> Result.map ~f:List.concat
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

let compare schema =
  let rec compare_json l r =
    let assoc_compare compare l r =
      let f (key, l) =
        match List.Assoc.find ~equal:String.equal r key with
        | None -> Some 1
        | Some r -> ( match compare l r with 0 -> None | v -> Some v)
      in
      Option.value ~default:0 @@ List.find_map ~f l
    in
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
    | Any (module Coding) -> Coding.compare
    | Bool -> Base.Bool.compare
    | Coding { f } -> compare (f None)
    | Date -> Base.Poly.compare
    | Fixpoint s -> compare (Lazy.force s)
    | Float -> Base.Poly.compare
    | Ignore -> Base.Poly.compare
    | Int | Int64 -> Base.Poly.compare
    | Json -> compare_json
    | List schema -> Base.List.compare (compare schema.descriptor)
    | Sequence { item; deconstruct; _ } ->
      fun l r ->
        Base.Sequence.compare (compare item.descriptor) (deconstruct l)
          (deconstruct r)
    | Map { encode; descriptor; _ } ->
      fun l r -> compare descriptor (encode l) (encode r)
    | Mapping { deconstruct; item; _ } ->
      fun l r ->
        let compare_field (key_l, value_l) (key_r, value_r) =
          match String.compare key_l key_r with
          | 0 -> compare item.descriptor value_l value_r
          | n -> n
        in
        Base.Sequence.compare compare_field (deconstruct l) (deconstruct r)
    | Null -> fun () () -> 0
    | Nullable s -> fun l r -> Base.Option.compare (compare s.descriptor) l r
    | String -> String.compare
    | String_const _ -> fun () () -> 0
    | Time -> Ptime.compare
    | Time_local -> (
      fun (l, lz) (r, rz) ->
        match Int.compare lz rz with 0 -> Ptime.compare l r | v -> v)
    | Object { fields; encode; decode = _ } ->
      let rec compare_fields : type u.
          u Hlist.t fields -> u Hlist.t -> u Hlist.t -> int = function
        | Field { field = { schema; requirement; _ }; rest } ->
          let schema = field_schema schema in
          let f (type field req) (schema : field t)
              (requirement : (field, req) requirement) Hlist.((l : req) :: lr)
              Hlist.((r : req) :: rr) =
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
        | FieldEnd -> fun Hlist.[] Hlist.[] -> 0
      in
      let compare_items = compare_fields fields in
      fun l r -> compare_items (encode l) (encode r)
    | Tuple { items; encode; decode = _ } ->
      let rec compare_items : type t. t tuple -> t -> t -> int = function
        | ItemEnd -> fun Hlist.[] Hlist.[] -> 0
        | Item { schema; rest } -> (
          fun Hlist.(l :: lr) Hlist.(r :: rr) ->
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

(* Object inlining *)

module Make_inline (M : Base.Monad.S) = struct
  include Make_inline (M)

  let ( let++ ) = M.Let_syntax.( >>| )
  let ( and++ ) = M.Let_syntax.Let_syntax.both

  let inline_try ~inline ~outline = function
    | Inline schema -> inline schema
    | Inline_try schema -> (
      match inline schema with
      | Result.Ok _ as ok -> ok
      | Result.Error _ -> outline schema)
    | Outline schema -> outline schema

  let schema desc f =
    let rec inline : type req res t.
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
        let rec inline_fields : type req f.
            (f Hlist.t, req) requirement ->
            f Hlist.t fields ->
            (res list list M.t, string) Result.t =
         fun req -> function
           | FieldEnd -> M.return [ [] ] |> Result.return
           | Field { field; rest } when field_ignored field -> (
             match req with
             | Computed -> inline_fields Computed rest
             | Computed_optional -> inline_fields Computed_optional rest
             | Default Hlist.(_ :: tail) -> inline_fields (Default tail) rest
             | Optional -> inline_fields Optional rest
             | Required -> inline_fields Required rest)
           | Field { field; rest } ->
             let+ head =
               let outline _schema =
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
                            since = field.since;
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
                            since = field.since;
                            title = field.title;
                          }
                      | Required, Default Hlist.(head :: _) ->
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
                            since = field.since;
                            title = field.title;
                          }
                    in
                    [ [ head ] ]
               and inline schema =
                 let next () =
                   match (field.requirement, req) with
                   | (Computed | Computed_optional | Default _ | Optional), _
                   | Required, Required ->
                     inline schema.descriptor field.requirement f
                   | Required, Default Hlist.(head :: _) ->
                     inline schema.descriptor (Default head) f
                   | Required, ((Computed | Optional) as requirement) ->
                     inline schema.descriptor requirement f
                   | Required, Computed_optional ->
                     inline schema.descriptor Computed_optional f
                 in
                 match f.filter with
                 | None -> next ()
                 | Some filter -> (
                   match filter field with
                   | None -> next ()
                   | Some res ->
                     Result.return
                     @@ let++ res in
                        (* For optional inline fields (like optional unions),
                           generate base case alternative *)
                        if is_optional field.requirement then [ [ res ]; [] ]
                        else [ [ res ] ])
               in
               inline_try ~inline ~outline field.schema
             and+ tail =
               match req with
               | Computed -> inline_fields Computed rest
               | Computed_optional -> inline_fields Computed_optional rest
               | Default Hlist.(_ :: tail) -> inline_fields (Default tail) rest
               | Optional -> inline_fields Optional rest
               | Required -> inline_fields Required rest
             in
             let++ head
             and++ tail in
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
                  since = None;
                  title = None;
                }
              |> M.map ~f:Option.return
          in
          let+ res = inline schema.descriptor Required f in
          let++ key
          and++ res in
          match key with
          | Some key -> List.map ~f:(fun l -> key :: l) res
          | None -> res
        in
        let res = List.map ~f cases in
        let+ res = Result.all res in
        let++ res = M.all res in
        let res = List.concat res in
        (* Add base case for optional unions to support None case *)
        if is_optional req then res @ [ [] ] else res
      | s ->
        Result.fail
          (Fmt.str "inline field is not an object: %a" pp_descriptor s)
    in
    inline desc Required f

  let value_versioned (f : 'res inline_v) fields =
    let rec inline_field : type req t.
        (t, req) field ->
        (req -> ('res list * Library_versions.t) M.t, string) Result.t =
     fun field ->
      let outline _schema =
        Result.return
        @@
        let f = f.f field in
        let version =
          match field.since with
          | None -> Library_versions.empty
          | Some (version, library) ->
            Library_versions.singleton library version
        in
        let with_version f v version =
          let++ res = f v in
          ([ res ], version)
        in
        match field.requirement with
        | Required -> fun v -> with_version f v version
        | Default default ->
          let equal = equal (field_schema field.schema) in
          fun v ->
            with_version f v
              (if equal v default then Library_versions.empty else version)
        | Computed -> (
          function
          | Some _ as v -> with_version f v version
          | None as v -> with_version f v Library_versions.empty)
        | Computed_optional -> (
          function
          | Some (Some _) as v -> with_version f v version
          | other -> with_version f other Library_versions.empty)
        | Optional -> (
          function
          | Some _ as v -> with_version f v version
          | None as v -> with_version f v Library_versions.empty)
      and inline :
          t Schema_intf.t ->
          (req -> ('res list * Library_versions.t) M.t, string) Result.t =
       fun schema ->
        let+ f = inline_descriptor schema.descriptor in
        match field.requirement with
        | Required -> (f : req -> ('res list * Library_versions.t) M.t)
        | Default _ -> f
        | Computed -> (
          function
          | Some v -> f v
          | None -> M.return ([], Library_versions.empty))
        | Computed_optional -> (
          function
          | Some (Some v) -> f v
          | _ -> M.return ([], Library_versions.empty))
        | Optional -> (
          function
          | Some v -> f v
          | None -> M.return ([], Library_versions.empty))
      in
      inline_try ~inline ~outline field.schema
    and inline_fields : type t.
        t Hlist.t fields ->
        (t Hlist.t -> ('res list * Library_versions.t) M.t, string) Result.t =
      function
      | FieldEnd ->
        Result.return (fun Hlist.[] -> M.return ([], Library_versions.empty))
      | Field { field; rest } ->
        let* tail = inline_fields rest in
        if field_ignored field then
          Result.return (fun Hlist.(_ :: tl) -> tail tl)
        else
          let+ head = inline_field field in
          fun Hlist.(hd :: tl) ->
            let++ head, head_version = head hd
            and++ tail, tail_version = tail tl in
            (head @ tail, Library_versions.union tail_version head_version)
    and inline_descriptor : type t.
        t descriptor ->
        (t -> ('res list * Library_versions.t) M.t, string) Result.t = function
      | Ignore ->
        Result.return (fun () -> M.return ([], Library_versions.empty))
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
        function Some v -> f v | None -> M.return ([], Library_versions.empty))
      | Object { fields; encode; _ } ->
        let+ f = inline_fields fields in
        fun v -> f (encode v)
      | Union { cases; key; _ } -> (
        let+ cases =
          let f (Case { encoder; name; schema; since; _ }) =
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
                       since = None;
                       title = None;
                     })
            in
            fun v ->
              match encoder v with
              | None -> None
              | Some v -> (
                let result =
                  match key with
                  | None -> res v
                  | Some key ->
                    let++ key = key ()
                    and++ res, versions = res v in
                    (key :: res, versions)
                in
                match since with
                | None -> Some result
                | Some (version, library) ->
                  Some
                    (let++ res, versions = result in
                     ( res,
                       Library_versions.with_version library version versions ))
                )
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

  let value f fields =
    let+ result = value_versioned f fields in
    fun fields ->
      let++ result, _ = result fields in
      result
end

module Inline = Make_inline (Base.Monad.Ident)

(* Walker *)

module Walk (W : WALK) = struct
  let ( let+ ) = W.M.Let_syntax.( >>| )
  let ( and+ ) = W.M.Let_syntax.Let_syntax.both

  module Inline = Make_inline (W.M)

  let rec descriptor : type t. id:string option -> t descriptor -> W.t W.M.t =
   fun ~id -> function
    | Any _ -> W.any
    | Bool -> W.bool
    | Coding { f } -> descriptor ~id (f None)
    | Float -> W.float
    | Date -> W.date
    | Fixpoint s -> descriptor ~id (Lazy.force s)
    | Int -> W.int
    | Ignore -> W.ignore
    | Int64 -> W.int64
    | Json -> W.json
    | List l -> W.list (schema l)
    | Null -> W.null
    | String -> W.string
    | Time -> W.time
    | Time_local -> W.time_local
    | Map { descriptor = d; _ } -> W.map @@ descriptor ~id d
    | Mapping { item; _ } -> W.mapping (schema item)
    | Nullable s -> W.nullable (schema s)
    | Sequence { item; _ } -> W.list (schema item)
    | String_const s -> W.string_const s
    | Object _ as o -> (
      let fields =
        let f { name; requirement; schema = s; description; since; title; _ } =
          W.field ~required:(required requirement)
            ~computed:(computed requirement) ?description
            ?since:(Option.map ~f:fst since) ?title name
            (schema @@ field_schema s)
        and filter field =
          let s = field_schema field.schema in
          W.inline
            (match s.parametric with
            | None -> None
            | Some p ->
              Some
                (fun () ->
                  let name, args, eff_args, t = parametric p in
                  W.parametric name args eff_args t))
            (fun () -> schema s)
            field
        in
        Inline.schema o { f; filter = Some filter }
      in
      match fields with Ok fields -> W.object_ fields | Error e -> failwith e)
    | Tuple { items; _ } -> W.tuple @@ tuple items
    | Union { cases; key; polymorphic } ->
      let cases = List.map ~f:(case key) cases |> W.M.all in
      W.union ~id ~polymorphic cases

  and case : type t. string option -> t case -> W.case W.M.t =
   fun key (Case { inline; name; raw_name; schema = s; since; _ }) ->
    W.case ?key ~inline ~name ~raw_name ?since:(Option.map ~f:fst since)
      (schema s)

  and schema : type t. t Schema_intf.t -> W.t W.M.t =
   fun { id; descriptor = d; parametric = p } ->
    let schema () = descriptor ~id d
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

  and parametric : type t.
      t parametric -> string * string list * W.t W.M.t list * W.t W.M.t =
    function
    | Plain { name; descriptor = d } -> (name, [], [], descriptor ~id:None d)
    | Parametric { name; parameter; next } ->
      let template_name, args, eff_args, t =
        parametric
          (next
             {
               id = Some ("#" ^ name);
               (* FIXME: Ensure we don't keep eg. an `Ignore` schema that would
                  be discarded. Handle that better with an actual `Var`
                  descriptor.*)
               descriptor =
                 Map
                   {
                     decode = (fun _ -> failwith "unreachable");
                     encode = (fun _ -> failwith "unreachable");
                     descriptor = Int;
                   };
               parametric = None;
             })
      in
      (template_name, name :: args, schema parameter :: eff_args, t)

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
  let result =
    Inline.schema descriptor
      {
        f =
          (fun { name; requirement; _ } ->
            Option.some_if (required requirement) name);
        filter = None;
      }
  in
  match result with
  | Result.Ok names -> Result.Ok (List.map ~f:(List.filter_map ~f:Fn.id) names)
  | Error e -> Error e

type pack = Pack : _ t -> pack

let pack (type a) (schema : a t) = Pack schema

type 'res unpacker = { unpack : 't. 't t -> 'res }

let unpack { unpack } (Pack schema) = unpack schema

let name_combine schema name =
  match schema.id with Some id -> id ^ "-" ^ name | None -> name

module Name = struct
  let ( let* ) = Option.( >>= )
  and ( let+ ) = Option.( >>| )

  let rec of_schema : type a. a t -> Name.t option = function
    | { id = Some id; _ } -> Some (Id id)
    | { parametric = Some parametric; _ } -> of_parametric parametric
    | { descriptor = Any (module Impl : Coding_intf.IMPL with type t = a); _ }
      ->
      Some (Id "any")
    | { descriptor = Bool; _ } -> Some (Id "bool")
    | { descriptor = Coding { f }; _ } as schema ->
      of_schema { schema with descriptor = f None }
    | { descriptor = Date; _ } -> Some (Id "date")
    | { descriptor = Fixpoint f; _ } as schema ->
      of_schema { schema with descriptor = Lazy.force f }
    | { descriptor = Float; _ } -> Some (Id "float")
    | { descriptor = Ignore; _ } -> Some (Id "ignore")
    | { descriptor = Int; _ } -> Some (Id "int")
    | { descriptor = Int64; _ } -> Some (Id "int64")
    | { descriptor = Json; _ } -> Some (Id "json")
    | { descriptor = List p; _ } ->
      let+ p = of_schema p in
      Name.Parametric ("list", [ p ])
    | { descriptor = Map _; _ } -> None
    | { descriptor = Mapping { item; _ }; _ } ->
      let+ p = of_schema item in
      Name.Parametric ("mapping", [ p ])
    | { descriptor = Null; _ } -> Some (Id "null")
    | { descriptor = Nullable p; _ } ->
      let+ p = of_schema p in
      Name.Parametric ("option", [ p ])
    | { descriptor = Object _; _ } -> None
    | { descriptor = Sequence { item; _ }; _ } ->
      let+ p = of_schema item in
      Name.Parametric ("list", [ p ])
    | { descriptor = String; _ } -> Some (Id "string")
    | { descriptor = String_const _; _ } -> None
    | { descriptor = Time; _ } -> Some (Id "time")
    | { descriptor = Time_local; _ } -> Some (Id "time_local")
    | { descriptor = Tuple _; _ } -> None
    | { descriptor = Union _; _ } -> None

  and of_parametric : type a. a parametric -> Name.t option =
   fun parametric ->
    let rec f = function
      | Plain { name; _ } -> Some (name, [])
      | Parametric { parameter; next; _ } ->
        let* parametric, parameters = f (next parameter) in
        let+ parameter = of_schema parameter in
        (parametric, parameter :: parameters)
    in
    let+ name, parameters = f parametric in
    Name.Parametric (name, parameters)

  include Name

  let rec sexp_of_t = function
    | Id s -> Sexp.Atom s
    | Parametric (name, ps) ->
      List [ Atom name; List (List.map ~f:sexp_of_t ps) ]

  include Comparable.Make (struct
    include Name

    let sexp_of_t = sexp_of_t
  end)

  let rec pp f = function
    | Id s -> Fmt.string f s
    | Parametric (name, p) -> Fmt.pf f "%s(%a)" name Fmt.(list ~sep:comma pp) p

  let to_string = Fmt.to_to_string pp

  let rec to_id = function
    | Id s -> s
    | Parametric (name, ps) ->
      (List.map ~f:to_id ps |> String.concat ~sep:"-") ^ "-" ^ name

  let combine schema id =
    let+ name = of_schema schema in
    to_id name ^ "-" ^ id
end

let fixpoint f =
  let rec self = Fixpoint (lazy (f self)) in
  f self

let atom v = Sexp.Atom v

and option_to_sexp ~f = function
  | None -> Sexp.List []
  | Some v -> Sexp.List [ f v ]

let rec to_sexp : type a. a t -> Sexp.t =
 fun { id; descriptor; _ } ->
  List
    [
      List [ Atom "id"; option_to_sexp ~f:atom id ];
      List [ Atom "descriptor"; descriptor_to_sexp descriptor ];
    ]

and descriptor_to_sexp : type a. a descriptor -> Sexp.t = function
  | Any (module Impl : Coding_intf.IMPL with type t = a) ->
    List [ Atom "Any"; Atom Impl.name ]
  | Bool -> Atom "Bool"
  | Coding { f } -> descriptor_to_sexp (f None)
  | Date -> Atom "Date"
  | Fixpoint f -> descriptor_to_sexp (Lazy.force f)
  | Float -> Atom "Float"
  | Ignore -> Atom "Ignore"
  | Int -> Atom "Int"
  | Int64 -> Atom "Int64"
  | Json -> Atom "Json"
  | List s -> List [ Atom "List"; to_sexp s ]
  | Sequence { item; _ } -> List [ Atom "List"; to_sexp item ]
  | Map { descriptor; _ } -> List [ Atom "Map"; descriptor_to_sexp descriptor ]
  | Mapping { item; _ } -> List [ Atom "Mapping"; to_sexp item ]
  | Null -> Atom "Null"
  | Nullable s -> List [ Atom "Nullable"; to_sexp s ]
  | Object { fields; _ } -> List (Atom "Object" :: fields_to_sexp fields)
  | String -> Atom "String"
  | String_const s -> List [ Atom "String_const"; Atom s ]
  | Time -> Atom "Time"
  | Time_local -> Atom "Time_local"
  | Tuple { items; _ } -> List (Atom "Tuple" :: items_to_sexp items)
  | Union { key; cases; _ } ->
    let tail = List.map ~f:case_to_sexp cases in
    let tail =
      match key with
      | Some key -> Sexp.List [ Atom "key"; Atom key ] :: tail
      | None -> tail
    in
    List (Atom "Union" :: tail)

and case_to_sexp : type f. f case -> Sexp.t =
 fun (Case { name; schema; _ }) -> List [ Atom name; to_sexp schema ]

and fields_to_sexp : type f. f fields -> Sexp.t list = function
  | FieldEnd -> []
  | Field { field = { name; schema; _ }; rest } ->
    List [ Atom name; to_sexp (field_schema schema) ] :: fields_to_sexp rest

and items_to_sexp : type f. f tuple -> Sexp.t list = function
  | ItemEnd -> []
  | Item { schema; rest } -> to_sexp schema :: items_to_sexp rest

let object_wrap name schema =
  {
    descriptor =
      Object
        {
          decode = (fun [ ok ] -> ok);
          encode = (fun ok -> [ ok ]);
          fields =
            Field
              {
                field =
                  {
                    description = None;
                    examples = [];
                    maximum = None;
                    minimum = None;
                    name;
                    omit = false;
                    requirement = Required;
                    schema = Outline schema;
                    since = None;
                    title = None;
                  };
                rest = FieldEnd;
              };
        };
    id = None;
    parametric = None;
  }

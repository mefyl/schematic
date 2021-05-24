open Base

module Mark = struct
  module T = struct
    type t = Schema : string * 'a Schema.t -> t

    let sexp_of_t (Schema (id, _)) = Sexp.Atom id

    let compare (Schema (l, _)) (Schema (r, _)) = String.compare l r
  end

  include T
  include Base.Comparable.Make (T)

  let make id schema = Schema (id, schema)

  let of_schema s = make (Option.value_exn ~here:[%here] s.Schema.id) s
end

module Marks = struct
  type t = (Mark.t, Mark.comparator_witness) Set.t

  let empty = Set.empty (module Mark)

  let make id s = Set.of_list (module Mark) [ Schema (id, s) ]

  let add = Set.add

  let merge = Set.union_list (module Mark)

  let has = Set.mem

  let of_list = Set.of_list (module Mark)
end

let name s =
  String.split ~on:'_' s
  |> List.map ~f:(String.split ~on:'-')
  |> List.concat
  |> List.map ~f:String.capitalize
  |> String.concat ~sep:""

type t =
  | Atom of string
  | Intersection of t list
  | Object of { fields : (string * t * bool) list }
  | String of string
  | Template of {
      name : string;
      args : t list;
    }
  | Tuple of t list
  | Union of t list

let rec of_descriptor : type s. s Schema.descriptor -> t * Marks.t = function
  | Any -> (Atom "any", Marks.empty)
  | Bool -> (Atom "boolean", Marks.empty)
  | Date -> (Atom "Date", Marks.empty)
  | Ignore -> (Atom "null", Marks.empty)
  | Int
  | Int64
  | Float ->
    (Atom "number", Marks.empty)
  | List s ->
    let s, marks = of_schema s in
    (Template { name = "Array"; args = [ s ] }, marks)
  | Map (_, _, d) -> of_descriptor d
  | Mapping s ->
    let s, marks = of_schema s in
    (Template { name = "Record"; args = [ Atom "string"; s ] }, marks)
  | Null -> (Atom "null", Marks.empty)
  | Nullable s ->
    let s, marks = of_schema s in
    (Union [ s; Atom "null" ], marks)
  | String -> (Atom "string", Marks.empty)
  | String_const s -> (Atom (Fmt.str "%S" s), Marks.empty)
  | Time
  | Time_local ->
    (Atom "Date", Marks.empty)
  | Object fields ->
    let fields, marks = of_object fields in
    (Object { fields }, marks)
  | Tuple items ->
    let items, marks = of_tuple items in
    (Tuple items, marks)
  | Union { cases; key } ->
    let cases = List.map ~f:(of_case key) cases in
    let cases = List.map ~f:fst cases
    and marks = Marks.merge @@ List.map ~f:snd cases in
    (Union cases, marks)

and of_schema : type s. s Schema.t -> t * Marks.t = function
  | { id = None; descriptor; _ } -> of_descriptor descriptor
  | { id = Some id; _ } as schema ->
    if String.is_prefix ~prefix:"#" id then
      (Atom (name @@ String.subo ~pos:1 id), Marks.empty)
    else
      (Atom (name id), Marks.make id schema)

and of_object : type s. s Schema.field -> (string * t * bool) list * Marks.t =
  function
  | Field
      {
        schema =
          Outline { descriptor = Ignore; _ } | Inline { descriptor = Ignore; _ };
        rest;
        _;
      } ->
    of_object rest
  | Field { field; schema = Outline schema; rest; requirement; omit; _ } ->
    if omit then
      of_object rest
    else
      let schema, marks = of_schema schema
      and rest, rest_marks = of_object rest in
      ( (field, schema, Schema.required requirement) :: rest,
        Marks.merge [ marks; rest_marks ] )
  | Field { schema = Inline { descriptor; _ }; rest; _ } ->
    let fields, marks =
      Schema.inline descriptor { f = (fun fields -> of_object fields) }
    and rest, rest_marks = of_object rest in
    (fields @ rest, Marks.merge [ marks; rest_marks ])
  | FieldEnd -> ([], Marks.empty)

and of_tuple : type s. s Schema.tuple -> t list * Marks.t = function
  | Item { schema; rest } ->
    let schema, marks = of_schema schema
    and rest, marks_rest = of_tuple rest in
    (schema :: rest, Marks.merge [ marks; marks_rest ])
  | ItemEnd -> ([], Marks.empty)

and of_case : type s. string option -> s Schema.case -> t * Marks.t =
 fun key (Case { name; schema; _ }) ->
  let schema, marks = of_schema schema in
  match key with
  | None -> (schema, marks)
  | Some key ->
    ( Intersection [ schema; Object { fields = [ (key, String name, true) ] } ],
      marks )

and of_parametric :
    type a.
    Marks.t -> string -> a Schema.parametric -> (string * t) list * Marks.t =
 fun marks id parametric ->
  let rec f params marks args = function
    | Schema.Parametric { parameter; next; name = param_name } ->
      let p, deps = of_schema parameter
      and param_name = name param_name in
      f (param_name :: params)
        (Marks.merge [ marks; deps ])
        (p :: args)
        (next { parameter with id = Some ("#" ^ param_name) })
    | Schema.Plain { name = template_name; descriptor } ->
      let schema =
        (name id, Template { name = name template_name; args = List.rev args })
      in
      let schemas, marks =
        let mark = Mark.make template_name (Schema.make descriptor) in
        if Marks.has marks mark then
          ([ schema ], marks)
        else
          let template, deps = of_descriptor descriptor
          and args = List.rev params in
          ( [
              schema;
              ( Fmt.(
                  str "%s<%a>" (name template_name)
                    (list ~sep:(const string ", ") string)
                    args),
                template );
            ],
            Marks.merge [ marks; deps ] )
      in
      (schemas, marks)
  in
  f [] marks [] parametric

let rec of_schema : type a. Marks.t -> a Schema.t -> (string * t) list * Marks.t
    =
 fun marks (Schema.{ id; descriptor; parametric } as schema) ->
  match (id, parametric) with
  | Some id, Some parametric -> of_parametric marks id parametric
  | Some id, None when Marks.has marks (Mark.make id schema) -> ([], marks)
  | Some id, None ->
    let res, deps = of_descriptor descriptor in
    let marks = Marks.add marks (Mark.make id schema) in
    let f (res, marks) (Mark.Schema (_, dep)) =
      let deps, marks = of_schema marks dep in
      (res @ deps, marks)
    in
    Set.fold ~init:([ (name id, res) ], marks) ~f deps
  | _ -> failwith "schema has no id"

let unpacker marks =
  let unpack : type a. a Schema.t -> (string * t) list * Marks.t =
   fun s -> of_schema marks s
  in
  { Schema.unpack }

let of_schemas ?(exclude = []) schemas =
  let f (res, marks) s =
    let ts, marks = Schema.unpack (unpacker marks) s in
    (res @ ts, marks)
  in
  List.fold_left
    ~init:
      ( [],
        Marks.of_list
          (List.map ~f:(Schema.unpack { unpack = Mark.of_schema }) exclude) )
    ~f schemas
  |> fst

let of_schema schema = of_schema Marks.empty schema |> fst

let of_descriptor d = of_descriptor d |> fst

let break sp id fmt _ = Caml.Format.pp_print_break fmt sp id

let custom_break fits breaks fmt _ =
  Caml.Format.pp_print_custom_break fmt ~fits:(fits, 0, "")
    ~breaks:(breaks, 0, "")

let pp ?(indent = 2) fmt ts =
  let rec pp fmt ts =
    let printer =
      let open Fmt in
      let tuple =
        const char '[' ++ break 1 indent
        ++ (hvbox @@ list ~sep:comma (hvbox pp))
        ++ sp ++ const char ']'
      and object_ =
        let field f (name, v, required) =
          let pp =
            box ~indent
            @@ Fmt.pair
                 ~sep:
                   (const string
                      (if required then
                        ":"
                      else
                        "?:")
                   ++ sp)
                 Dump.string (hvbox pp)
          in
          pp f (name, v)
        in
        const char '{' ++ break 1 indent
        ++ (hvbox @@ list ~sep:semi field)
        ++ sp ++ const char '}'
      and union = hvbox @@ list ~sep:(custom_break " | " " |") (hvbox pp) in
      match ts with
      | Atom s -> const string s
      | Intersection types ->
        const (list ~sep:(sp ++ const char '&' ++ sp) pp) types
      | Object { fields; _ } -> const object_ fields
      | String s -> const Dump.string s
      | Template { name; args } ->
        hbox
        @@ const string name ++ const char '<'
           ++ const (list ~sep:comma pp) args
           ++ const char '>'
      | Tuple l -> const tuple l
      | Union l ->
        (* FIXME: parenthesizing ? *)
        const union l
    in
    printer fmt ()
  in
  pp fmt ts

let to_string t = Fmt.(str "%a" (box pp) t)

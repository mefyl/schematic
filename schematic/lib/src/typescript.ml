open Base

(** A TypeScript type expression. *)
type t =
  | Atom of string  (** An identifier. *)
  | Intersection of t list  (** [X & Y & ... ] *)
  | Object of { fields : (string * t * bool) list }
      (** An object, with fields defined as [(name, type, required)] *)
  | String of string  (** A string literal *)
  | Template of {
      name : string;
      args : t list;
    }  (** [Template<T1, T2, ..., TN>] *)
  | Tuple of t list  (** [\[T1, T2, ..., TN \]]*)
  | Union of t list  (** [T1 | T2 | ... | TN] *)

type typescript = t

let name s =
  String.split ~on:'_' s
  |> List.map ~f:(String.split ~on:'-')
  |> List.concat
  |> List.map ~f:String.capitalize
  |> String.concat ~sep:""

include Marks.Make (struct
  type t = typescript

  (* TODO: use type-safe ID for Typescript as well as C *)
  module Id = struct
    include String

    let make s = name s
  end
end)

module Of = Schema.Walk (struct
  type res = t * Marks.t
  type fields = (string * t * bool) list list * Marks.t
  type t = res

  (* Schema *)

  let schema id parametric s =
    match parametric with
    | Some parametric -> parametric ()
    | None -> (
      match id with
      | None -> s ()
      | Some id ->
        if String.is_prefix ~prefix:"#" id then
          (Atom (name @@ String.subo ~pos:1 id), Marks.empty)
        else
          let mark = Marks.make id s in
          (Atom (name id), mark))

  let parametric template_name args effective t =
    let template_name = name template_name in
    let effective, marks = List.unzip effective in
    let mark =
      let id =
        Fmt.(
          str "%s<%a>" template_name
            (list ~sep:(const string ", ") string)
            (List.map ~f:name args))
      in
      (id, lazy t)
    in
    ( Template { name = template_name; args = effective },
      Marks.add (Marks.merge marks) mark )

  let make name = (Atom name, Marks.empty)
  let any () = make "any"
  let bool () = make "boolean"
  let fields_empty () = ([ [] ], Marks.empty)

  let field_inline inline_required cases (rest, rest_marks) =
    let fields, marks = List.unzip cases in
    let fields = Base.Sequence.(of_list fields |> map ~f:of_list |> concat) in
    let fields =
      Base.Sequence.(
        cartesian_product fields (of_list rest)
        |> map ~f:(fun (l, r) ->
               List.map
                 ~f:(fun (name, t, required) ->
                   (name, t, required && inline_required))
                 l
               @ r)
        |> to_list)
    in
    (fields, Marks.merge (rest_marks :: marks))

  let field_outline name required (schema, marks) (rest, rest_marks) =
    Fmt.epr "field_outline %S %b@." name required;
    ( List.map ~f:(List.cons (name, schema, required)) rest,
      Marks.merge [ marks; rest_marks ] )

  let float () = make "number"
  let date () = make "Date"
  let int = float
  let ignore () = make "null"
  let int64 = float
  let list (s, marks) = (Template { name = "Array"; args = [ s ] }, marks)

  let mapping (elt, marks) =
    (Template { name = "Record"; args = [ Atom "string"; elt ] }, marks)

  let null () = make "null"
  let nullable (t, marks) = (Union [ t; Atom "null" ], marks)

  let object_ (fields_cases, marks) =
    let f fields = Object { fields } in
    match fields_cases with
    | [] -> failwith "typescript: of_objects yielded no result"
    | [ fields ] -> (f fields, marks)
    | cases -> (Union (List.map ~f cases), marks)

  let string () = make "string"
  let time () = make "Date"
  let time_local = time

  let tuple items =
    let items, marks = List.unzip items in
    (Tuple items, Marks.merge marks)

  let string_const s = (Atom (Fmt.str "%S" s), Marks.empty)

  (* Union *)

  let case key name (schema, marks) =
    match key with
    | None -> (schema, marks)
    | Some key ->
      ( Intersection [ schema; Object { fields = [ (key, String name, true) ] } ],
        marks )

  let union cases =
    let t, marks = List.unzip cases in
    (Union t, Marks.merge marks)
end)

let mark_of_schema Schema.{ id; descriptor; parametric } =
  match parametric with
  | None -> (Option.value_exn ~here:[%here] id, lazy (Of.descriptor descriptor))
  | Some parametric ->
    (Option.value_exn ~here:[%here] id, lazy (Of.parametric parametric))

let of_schema marks s = of_mark marks @@ mark_of_schema s

let unpacker marks =
  let unpack : type a. a Schema.t -> (string * t) list * Marks.t =
   fun s -> of_schema marks s
  in
  { Schema.unpack }

let of_schemas_state marks schemas =
  let f (res, marks) s =
    let ts, marks = Schema.unpack (unpacker marks) s in
    (res @ ts, marks)
  in
  List.fold_left ~init:([], marks) ~f schemas

let state_exclude schemas =
  Marks.of_list
    (List.map ~f:(Schema.unpack { unpack = mark_of_schema }) schemas)

let of_schemas ?(exclude = []) schemas =
  let marks = state_exclude exclude in
  of_schemas_state marks schemas |> fst

let of_schema schema = of_schema Marks.empty schema |> fst
let of_descriptor d = Of.descriptor d |> fst
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
          Fmt.epr "field %S %b@." name required;
          let pp =
            box ~indent
            @@ Fmt.pair
                 ~sep:(const string (if required then ":" else "?:") ++ sp)
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

type state = Marks.t

let state_empty = Marks.empty

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
[@@deriving show]

type typescript = t

let name s =
  String.split ~on:'_' s
  |> List.map ~f:(String.split ~on:'-')
  |> List.concat
  |> List.map ~f:String.capitalize
  |> String.concat ~sep:""

include Marks.Make (struct
  type t = typescript

  let pp = pp

  module Id = struct
    include String

    let make s = name s
  end
end)

module Of = Schema.Walk (struct
  type field = string * t * bool
  type nonrec t = t

  module M = State_monad.Make (struct
    type t = Marks.t
  end)

  let ( let* ) = M.Let_syntax.( >>= )
  let ( and* ) = M.Let_syntax.Let_syntax.both
  let ( let+ ) = M.Let_syntax.( >>| )

  (* Schema *)

  let schema id parametric s marks =
    match parametric with
    | Some parametric -> parametric () marks
    | None -> (
      match id with
      | None -> s () marks
      | Some id ->
        if String.is_prefix ~prefix:"#" id then
          (Atom (name @@ String.subo ~pos:1 id), Marks.empty)
        else
          let mark = Marks.make id @@ fun () -> s () marks in
          (Atom (name id), mark))

  let parametric template_name args effective t =
    let* t = t
    and* effective = M.all effective in
    fun marks ->
      let template_name = name template_name in
      let mark =
        let id =
          Fmt.(
            str "%s<%a>" template_name
              (list ~sep:(const string ", ") string)
              (List.map ~f:name args))
        in
        (id, lazy (t, marks))
      in
      (Template { name = template_name; args = effective }, Marks.add marks mark)

  let atom name marks = (Atom name, marks)
  let any = atom "any"
  let bool = atom "boolean"

  let field name required schema =
    let+ schema = schema in
    (name, schema, required)

  let inline _ _ = None
  let float = atom "number"
  let date = atom "Date"
  let int = float
  let ignore = atom "null"
  let int64 = float

  let list elt =
    let+ elt = elt in
    Template { name = "Array"; args = [ elt ] }

  let mapping elt =
    let+ elt = elt in
    Template { name = "Record"; args = [ Atom "string"; elt ] }

  let null = atom "null"

  let nullable schema =
    let+ schema = schema in
    Union [ schema; Atom "null" ]

  let object_ fields =
    let f fields = Object { fields } in
    let+ fields = fields in
    match fields with
    | [] -> failwith "typescript: of_objects yielded no result"
    | [ fields ] -> f fields
    | cases -> Union (List.map ~f cases)

  let string = atom "string"
  let time = atom "Date"
  let time_local = time

  let tuple items =
    let+ items = items in
    Tuple items

  let string_const s marks = (Atom (Fmt.str "%S" s), marks)

  (* Union *)

  let case key name schema =
    let+ schema = schema in
    match key with
    | None -> schema
    | Some key ->
      Intersection [ schema; Object { fields = [ (key, String name, true) ] } ]

  let union cases =
    let+ cases = cases in
    Union cases
end)

include Make_mark_converter (struct
  let of_descriptor _ descriptor = Of.descriptor descriptor Marks.empty
  let of_parametric parametric = Of.parametric parametric Marks.empty
end)

let of_descriptor d = Of.descriptor d Marks.empty |> fst
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

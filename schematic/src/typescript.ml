open Base

(** A TypeScript type expression. *)
type field = {
  name : string;
  type' : t;
  required : bool;
  description : string option;
  since : Version.t option;
  title : string option;
}

and t =
  | Atom of string  (** An identifier. *)
  | Intersection of t list  (** [X & Y & ... ] *)
  | Object of { fields : field list }  (** An object with typed fields *)
  | String of string  (** A string literal *)
  | Template of {
      name : string;
      args : t list;
    }  (** [Template<T1, T2, ..., TN>] *)
  | Tuple of t list  (** [[T1, T2, ..., TN ]]*)
  | Union of case list  (** [T1 | T2 | ... | TN] *)

and case = {
  since : Version.t option;
  t : t;
  documentation : string option;
}
[@@deriving show]

let union cases =
  Union (List.map ~f:(fun t -> { since = None; t; documentation = None }) cases)

type documented = {
  type_ : t;
  documentation : string option;
}

let name s =
  String.split ~on:'_' s
  |> List.map ~f:(String.split ~on:'-')
  |> List.concat
  |> List.map ~f:String.capitalize
  |> String.concat ~sep:""

include Marks.Make (struct
  type t = documented

  let pp fmt { type_; documentation = _ } = pp fmt type_

  module Id = struct
    include String

    let make s = name s
  end
end)

module Of = Schema.Walk (struct
  type nonrec field =
    | Field of field
    | Type of t

  type nonrec t = t
  type nonrec case = case

  module M = State_monad.Make (struct
    type t = Marks.t
  end)

  let ( let* ) = M.Let_syntax.( >>= )
  let ( and* ) = M.Let_syntax.Let_syntax.both
  let ( let+ ) = M.Let_syntax.( >>| )

  (* Schema *)

  let of_id id documentation s =
    if String.is_prefix ~prefix:"#" id then
      M.return (Atom (name @@ String.subo ~pos:1 id))
    else fun marks ->
      let mark =
        ( id,
          lazy
            (let type_, marks = s () marks in
             ({ type_; documentation }, marks)) )
      in
      (Atom (name id), Marks.add marks mark)

  let schema ~id ~documentation parametric s =
    match parametric with
    | Some parametric -> parametric ()
    | None -> (
      match id with None -> s () | Some id -> of_id id documentation s)

  let parametric template_name args effective type_ =
    let* type_
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
        (id, lazy ({ type_; documentation = None }, marks))
      in
      (Template { name = template_name; args = effective }, Marks.add marks mark)

  let atom name marks = (Atom name, marks)
  let any = atom "any"
  let json = atom "any"
  let bool = atom "boolean"

  let field ~computed:_ ~required ?description ?since ?title name schema =
    let+ schema in
    Field { name; type' = schema; required; description; since; title }

  let inline (parametric : (unit -> Marks.t -> t * Marks.t) option) schema s =
    match (parametric, s) with
    | _, Schema.{ schema = Inline { id = Some _; _ }; _ } ->
      Option.return
      @@ let+ t = schema () in
         Type t
    | Some parametric, _ ->
      Some
        (fun marks ->
          let t, marks = parametric () marks in
          (Type t, marks))
    | _ -> None

  let float = atom "number"
  let date = atom "Date"
  let int = float
  let ignore marks = (Object { fields = [] }, marks)
  let int64 = float

  let list elt =
    let+ elt in
    Template { name = "Array"; args = [ elt ] }

  let map ts = ts

  let mapping elt =
    let+ elt in
    Template { name = "Record"; args = [ Atom "string"; elt ] }

  let null = atom "null"

  let nullable schema =
    let+ schema in
    union [ schema; Atom "null" ]

  let object_ fields =
    let f fields =
      let fields, types =
        List.partition_map fields ~f:(function
          | Field f -> First f
          | Type t -> Second t)
      in
      match (fields, types) with
      | [], [] -> Object { fields = [] }
      | [], [ t ] -> t
      | [], types -> Intersection types
      | fields, [] -> Object { fields }
      | fields, types -> Intersection (Object { fields } :: types)
    in
    let+ fields in
    match fields with
    | [] -> failwith "typescript: of_objects yielded no result"
    | [ fields ] -> f fields
    | cases -> union (List.map ~f cases)

  let string = atom "string"
  let time = atom "Date"
  let time_local = time

  let tuple items =
    let+ items in
    Tuple items

  let string_const s marks = (Atom (Fmt.str "%S" s), marks)

  (* Union *)

  let case ?key ~inline:_ ~name ~raw_name:_ ?since ?documentation schema =
    let+ t =
      let+ schema in
      match key with
      | None -> schema
      | Some key -> (
        let key =
          Object
            {
              fields =
                [
                  {
                    name = key;
                    type' = String name;
                    required = true;
                    description = None;
                    since = None;
                    title = None;
                  };
                ];
            }
        in
        match schema with
        | Object { fields = [] } -> key
        | _ -> Intersection [ schema; key ])
    in
    { since; t; documentation }

  let union ~id:_ ~polymorphic:_ cases =
    let+ cases in
    Union cases
end)

include Make_mark_converter (struct
  let identifier { Schema.id; _ } _ = Option.value_exn ~here:[%here] id

  let convert ~id ~documentation descriptor parametric =
    let type_, marks =
      match parametric with
      | Some parametric -> Of.parametric parametric Marks.empty
      | _ -> Of.descriptor ~id descriptor Marks.empty
    in
    ({ type_; documentation }, marks)
end)

let of_descriptor d = Of.descriptor ~id:None d Marks.empty |> fst

(** Escape documentation strings for JSDoc comments. Replaces */ with *\/ to
    avoid prematurely closing comment blocks. *)
let escape_jsdoc_comment s =
  String.substr_replace_all s ~pattern:"*/" ~with_:"*\\/"

let pp_docstring fmt = function
  | None -> ()
  | Some doc ->
    let escaped = escape_jsdoc_comment doc in
    Fmt.pf fmt "@[<hv 2>/**@ @[<h>%a@]@ @]*/@;" Fmt.string escaped

let break sp id fmt _ = Stdlib.Format.pp_print_break fmt sp id

let custom_break fits breaks fmt _ =
  Stdlib.Format.pp_print_custom_break fmt ~fits:(fits, 0, "")
    ~breaks:(breaks, 0, "")

module Precedence = struct
  module T = struct
    type t =
      | Delimited
      | Intersection
      | Union
    [@@deriving show { with_path = false }]

    let to_string = Fmt.to_to_string pp
    let to_int = function Delimited -> 0 | Intersection -> 2 | Union -> 1
    let compare l r = Int.compare (to_int l) (to_int r)
    let sexp_of_t t = Sexp.Atom (to_string t)
  end

  include T
  include Comparable.Make (T)
end

let pp ?(annotate_since = false) ?(indent = 2) fmt ts =
  let rec pp (prec : Precedence.t) fmt ts =
    let since_comment =
      let open Fmt in
      if annotate_since then
        option @@ (const string " /* since " ++ Version.pp ++ const string " */")
      else nop
    in
    let printer =
      let open Fmt in
      let tuple =
        const char '['
        ++ break 1 indent
        ++ (hvbox @@ list ~sep:comma (hvbox (pp Delimited)))
        ++ sp
        ++ const char ']'
      and object_ =
        let field f { name; type'; required; since; description; _ } =
          pp_docstring f description;
          let pp =
            box ~indent
            @@ Fmt.pair
                 ~sep:(const string (if required then ":" else "?:") ++ sp)
                 Dump.string
                 (hvbox (pp Delimited))
               ++ const since_comment since
          in
          pp f (name, type')
        in
        const char '{'
        ++ break 1 indent
        ++ (hvbox @@ list ~sep:semi field)
        ++ sp
        ++ const char '}'
      and union =
        let pp =
          let pp_case f { since; t; documentation } =
            Fmt.pf f "%a%a%s%a" (pp Union) t since_comment since
              (if Option.is_some documentation then " " else "")
              pp_docstring documentation
          in
          hvbox @@ list ~sep:(custom_break " | " " |") (hvbox pp_case)
        in
        if Precedence.(Union < prec) then const char '(' ++ pp ++ const char ')'
        else pp
      in
      match ts with
      | Atom s -> const string s
      | Intersection types ->
        const (list ~sep:(sp ++ const char '&' ++ sp) (pp Intersection)) types
      | Object { fields; _ } -> const object_ fields
      | String s -> const Dump.string s
      | Template { name; args } ->
        hbox
        @@ const string name
           ++ const char '<'
           ++ const (list ~sep:comma (pp Delimited)) args
           ++ const char '>'
      | Tuple l -> const tuple l
      | Union cases -> const union cases
    in
    printer fmt ()
  in
  pp Union fmt ts

let to_string ?annotate_since ?indent t =
  Fmt.(str "%a" (box (pp ?annotate_since ?indent)) t)

(** [pp_toplevel fmt (name, (ts, doc))] prints a toplevel definition defining
    [name] as [ts], with optional JSDoc documentation. *)
let pp_toplevel ?annotate_since ?indent fmt (name, { type_; documentation }) =
  pp_docstring fmt documentation;
  Fmt.pf fmt "@[<2>export type %s =@ %a;@]" name
    (fun fmt -> pp ?annotate_since ?indent fmt)
    type_

type state = Marks.t

let state_empty = Marks.empty

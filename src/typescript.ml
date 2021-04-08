let name s =
  String.split_on_char '_' s
  |> List.map (String.split_on_char '-')
  |> List.concat
  |> List.map String.capitalize_ascii
  |> String.concat ""

type t =
  | Atom of string
  | Intersection of t list
  | Object of {
      name : string option;
      fields : (string * t * bool) list;
    }
  | String of string
  | Template of {
      name : string;
      args : t list;
    }
  | Tuple of t list
  | Union of t list

let rec of_descriptor : type s. string option -> s Schema.descriptor -> t =
 fun name -> function
  | Any -> Atom "any"
  | Bool -> Atom "boolean"
  | Date -> Atom "Date"
  | Ignore -> Atom "null"
  | Int
  | Int64
  | Float ->
    Atom "number"
  | List s -> Template { name = "Array"; args = [ of_schema s ] }
  | Map (_, _, d) -> of_descriptor name d
  | Mapping s ->
    Template { name = "Record"; args = [ Atom "string"; of_schema s ] }
  | Null -> Atom "null"
  | Nullable s -> Union [ of_schema s; Atom "null" ]
  | String -> Atom "string"
  | String_const s -> Atom (Fmt.str "%S" s)
  | Time
  | Time_local ->
    Atom "Date"
  | Object fields -> Object { name; fields = of_object fields }
  | Tuple items -> Tuple (of_tuple items)
  | Union { cases; key } -> Union (List.map (of_case key) cases)

and of_schema : type s. s Schema.t -> t = function
  | { id = None; descriptor } -> of_descriptor None descriptor
  | { id = Some id; _ } -> Atom (name id)

and of_object : type s. s Schema.field -> (string * t * bool) list = function
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
      (field, of_schema schema, Schema.required requirement) :: of_object rest
  | Field { schema = Inline { descriptor; _ }; rest; _ } ->
    Schema.inline descriptor { f = (fun fields -> of_object fields) }
    @ of_object rest
  | FieldEnd -> []

and of_tuple : type s. s Schema.tuple -> t list = function
  | Item { schema; rest } -> of_schema schema :: of_tuple rest
  | ItemEnd -> []

and of_case : type s. string option -> s Schema.case -> t =
 fun key (Case { name; schema; _ }) ->
  let schema = of_schema schema in
  match key with
  | None -> schema
  | Some key ->
    Intersection
      [ schema; Object { name = None; fields = [ (key, String name, true) ] } ]

let of_schema Schema.{ id; descriptor } = of_descriptor id descriptor

let break sp id fmt _ = Format.pp_print_break fmt sp id

let custom_break fits breaks fmt _ =
  Format.pp_print_custom_break fmt ~fits:(fits, 0, "") ~breaks:(breaks, 0, "")

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

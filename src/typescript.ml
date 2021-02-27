open Schema

let name s =
  String.split_on_char '_' s
  |> List.map (String.split_on_char '-')
  |> List.concat
  |> List.map String.capitalize_ascii
  |> String.concat ""

type t =
  | Atom of string
  | Object of {
      name : string option;
      fields : (string * t * bool) list;
    }
  | Template of {
      name : string;
      args : t list;
    }
  | Tuple of t list
  | Union of t list

let rec of_descriptor :
    type s. string option -> (s, [> `Value ]) descriptor -> t =
 fun name -> function
  | Any -> Atom "any"
  | Bool -> Atom "boolean"
  | Date -> Atom "Date"
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
  | Object o -> Object { name; fields = of_object (Object o) }
  | Tuple t -> Tuple (of_tuple (Tuple t))
  | Union cases -> Union (List.map of_case cases)

and of_schema : type s. s schema -> t = function
  | { id = None; descriptor } -> of_descriptor None descriptor
  | { id = Some id; _ } -> Atom (name id)

and of_object : type s. (s, [ `Object ]) descriptor -> (string * t * bool) list
    = function
  | Object { field; schema = Outline schema; rest; requirement; omit; _ } ->
    if omit then
      of_object rest
    else
      (field, of_schema schema, Schema.required requirement) :: of_object rest
  | Object { schema = Inline { descriptor = Object o; _ }; rest; _ } ->
    of_object (Object o) @ of_object rest
  | Object { schema = Inline { descriptor = Map (_, _, Object o); _ }; rest; _ }
    ->
    of_object (Object o) @ of_object rest
  | Object { schema = Inline s; _ } ->
    Fmt.failwith "invalid inline schema: %a" Schema.pp s
  | Null -> []

and of_tuple : type s. (s, [ `Tuple ]) descriptor -> t list = function
  | Tuple { schema; rest } -> of_schema schema :: of_tuple rest
  | Null -> []

and of_case : type s. s case -> t = fun (Case { schema; _ }) -> of_schema schema

let of_schema { id; descriptor } = of_descriptor id descriptor

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
      | Object { fields; _ } -> const object_ fields
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

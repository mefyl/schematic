open Batteries

type t = Json_repr.ezjsonm

open Schema

let decode s = Json_encoding.destruct !s

let encode s = Json_encoding.construct !s

let from_channel = Ezjsonm.value_from_channel

let of_string = Ezjsonm.value_from_string

let to_channel chan t = Ezjsonm.value_to_channel ~minify:true chan t

let to_string t = Ezjsonm.value_to_string ~minify:true t

let of_schema s = Json_encoding.schema !s |> Json_schema.to_json

let rec pp fmt : t -> unit =
  let iter_tail head tail = function
    | [] -> ()
    | [ hd ] -> head hd
    | hd :: tl ->
      head hd;
      List.iter tail tl
  in
  let open Format in
  let indent = 2 in
  function
  | `Bool b -> fprintf fmt "%b" b
  | `Float f -> fprintf fmt "%f" f
  | `String s -> fprintf fmt "\"%s\"" s (* FIXME: escape *)
  | `Null -> fprintf fmt "null"
  | `A elements ->
    let f e =
      pp_print_string fmt ",";
      pp_print_space fmt ();
      pp fmt e
    in
    pp_print_string fmt "[";
    pp_open_hvbox fmt indent;
    iter_tail (pp fmt) f elements;
    pp_close_box fmt ();
    pp_print_string fmt "]"
  | `O elements ->
    let pp_field (name, value) =
      fprintf fmt "\"%s\":" name;
      (* FIXME: escape *)
      pp_print_space fmt ();
      pp fmt value
    in
    let f field =
      pp_print_string fmt ",";
      pp_print_space fmt ();
      pp_field field
    in
    pp_print_string fmt "{";
    pp_open_hvbox fmt indent;
    iter_tail pp_field f elements;
    pp_close_box fmt ();
    pp_print_string fmt "}"

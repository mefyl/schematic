type decoding = {
  path : string list;
  schema : string option;
  reason : string;
}
[@@deriving eq]

let pp_decoding formatter { path; schema; reason } =
  Fmt.(
    pf formatter "path %a: %a%s"
      (list ~sep:(const char '.') string)
      (List.rev path)
      (option (const string "schema " ++ string ++ const string ": "))
      schema reason)

let decoding_to_string = Fmt.to_to_string pp_decoding

type parsing = string [@@deriving eq]

let pp_parsing = Fmt.string

let parsing_to_string = Fmt.to_to_string pp_parsing

type 'formats t =
  [ `Decoding of 'formats * decoding
  | `Parsing of parsing
  ]

let of_decoding fmt e = `Decoding (fmt, e)

let of_parsing e = `Parsing e

let pp fmt = function
  | `Parsing e -> pp_parsing fmt e
  | `Decoding (_, e) -> pp_decoding fmt e

let to_string e = Fmt.str "%a" pp e

let equal a b =
  match (a, b) with
  | `Decoding (_, a), `Decoding (_, b) -> equal_decoding a b
  | `Parsing a, `Parsing b -> equal_parsing a b
  | `Decoding _, `Parsing _ -> false
  | `Parsing _, `Decoding _ -> false

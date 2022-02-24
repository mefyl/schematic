type decoding = {
  path : string list;
  schema : string option;
  reason : string;
}
[@@deriving eq]

let pp_decoding formatter { path; schema; reason } =
  let path = match path with [] -> None | _ -> Some (List.rev path) in
  Fmt.(
    pf formatter "%a%a%s"
      (option
         (const string "path "
         ++ list ~sep:(const char '.') string
         ++ const string ": "))
      path
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
[@@deriving eq]

let of_decoding fmt e = `Decoding (fmt, e)
let of_parsing e = `Parsing e

let pp fmt = function
  | `Parsing e -> pp_parsing fmt e
  | `Decoding (_, e) -> pp_decoding fmt e

let to_string e = Fmt.str "%a" pp e

type http = {
  body : string option;
  method_ : Cohttp.Code.meth;
  reason : string option;
  status : Cohttp.Code.status_code;
  uri : Uri.t; [@main]
}
[@@deriving make]

type t =
  [ `Assert of string
  | `Coding of Schematic.Error.decoding
  | `Networking of http
  ]

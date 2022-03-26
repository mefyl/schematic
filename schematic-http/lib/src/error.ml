type 'a t = {
  method_ : Cohttp.Code.meth;
  reason : 'a;
  status : Cohttp.Code.status_code option;
  uri : Uri.t; [@main]
}
[@@deriving make]

type reason =
  [ `Assert of string
  | `Coding of Schematic.Error.decoding
  | `Networking
  ]

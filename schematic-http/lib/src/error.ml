type never = |

type 'a reason =
  | Decoding of Schematic.Error.decoding
  | Http of 'a
  | Networking

type 'a t = {
  method_ : Cohttp.Code.meth;
  reason : 'a reason;
  status : Cohttp.Code.status_code option;
  uri : Uri.t; [@main]
}
[@@deriving make]

type ('a, 'b) status =
  | Unexpected_status of 'a
  | Valid_status of 'b

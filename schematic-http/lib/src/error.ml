type never = |

and 'a reason =
  [ `Decoding of Schematic.Error.decoding
  | `Http of 'a
  | `Networking
  ]

and ('a, 'b) status =
  | Unexpected_status of 'a
  | Valid_status of 'b
[@@deriving eq]

type 'a t = {
  method_ : Cohttp.Code.meth; [@equal ( = )]
  reason : 'a;
  status : Cohttp.Code.status_code option; [@equal ( = )]
  uri : Uri.t; [@main]
}
[@@deriving eq, make]

let pp_status unexpected valid f =
  let open Fmt in
  function
  | Unexpected_status s -> pf f "unexpected status: %a" unexpected s
  | Valid_status v -> valid f v

let pp_reason pp f =
  let open Fmt in
  function
  | `Decoding e -> pf f "decoding error: %a" Schematic.Error.pp_decoding e
  | `Http e -> pp f e
  | `Networking -> string f "connectivity or CORS error"

let pp pp_reason f { method_; reason; status = _; uri } =
  let printer =
    let open Fmt in
    box ~indent:2
    @@ const string Cohttp.Code.(string_of_method method_)
       ++ const char ' '
       ++ const Uri.pp uri
       ++ const char ':'
       ++ sp
       ++ const pp_reason reason
  in
  printer f ()

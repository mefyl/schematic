type never = |

type 'a t = {
  method_ : Http.Method.t; [@equal ( = )]
  reason : 'a;
  status : Http.Status.t option; [@equal ( = )]
  uri : Uri.t; [@main]
}
[@@deriving eq, make]

type reason =
  [ `Decoding of Schematic.Error.decoding
  | `Networking
  ]

let pp_reason f =
  let open Fmt in
  function
  | `Decoding e -> pf f "decoding error: %a" Schematic.Error.pp_decoding e
  | `Networking -> string f "connectivity or CORS error"

let pp pp_reason f { method_; reason; status; uri } =
  let printer =
    let open Fmt in
    box ~indent:2
    @@ const string (Http.Method.to_string method_)
       ++ const char ' '
       ++ const Uri.pp uri
       ++ const
            (option (const string " (" ++ Http.Status.pp ++ const char ')'))
            status
       ++ const char ':'
       ++ sp
       ++ const pp_reason reason
  in
  printer f ()

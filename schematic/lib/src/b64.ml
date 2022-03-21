module E = Error
open Base

module Make (C : Coding.S with type t = string) :
  Coding.S with type t = string = struct
  type t = string

  let alphabet = Base64.uri_safe_alphabet

  let decode schema s =
    match Base64.decode ~alphabet s with
    | Result.Ok s -> C.decode schema s
    | Result.Error (`Msg err) ->
      Result.Error
        E.
          {
            path = [];
            schema = None;
            reason = "Base 64 decoding failed: " ^ err;
          }

  let encode schema value = C.encode schema value |> Base64.encode_exn ~alphabet
end

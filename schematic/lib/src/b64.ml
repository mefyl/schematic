module Error_ = Error
open Base
include Base64_intf

module Decoder : Decoder_Impl = struct
  let alphabet = Base64.uri_safe_alphabet

  let of_string b64_str =
    Base64.decode ~alphabet b64_str
    |> Result.map_error ~f:(fun (`Msg err) -> "Base 64 decoding failed: " ^ err)

  let to_string str =
    (* encode_exn can only raise when passed arguments we are not using. *)
    Base64.encode_exn ~alphabet str
end

module Make (Decoder : Decoder_Impl) (Backend : Backend_Impl) = struct
  let ( >>= ) = Result.( >>= )
  let equal = String.equal
  let pp = String.pp
  let of_string s = Decoder.of_string s
  let to_string t = Decoder.to_string t

  let decode schema b64_str =
    of_string b64_str >>= Backend.of_string
    |> Result.map_error ~f:(fun err ->
           {
             Error_.path = [];
             schema = None;
             reason = "string decoding error:" ^ err;
           })
    >>= Backend.decode schema

  let encode schema value =
    Backend.encode schema value |> Backend.to_string |> to_string
end

include Make (Decoder) (Json)
type decode_error = [ `Json_decode of string ]

type parse_error = [ `Parse of string ]

type t = Ezjsonm.value

module type S = sig
  open Base
  open Schema

  val decode : 'a schema -> t -> ('a, [> decode_error ]) Result.t

  val encode : 'a schema -> 'a -> t

  val from_channel : Stdio.In_channel.t -> (t, [> parse_error ]) Result.t

  val to_channel : Stdio.Out_channel.t -> t -> unit

  val of_string : string -> (t, [> parse_error ]) Result.t

  val to_string : t -> string

  val pp : Formatter.t -> t -> unit

  val to_ezjsonm : t -> Ezjsonm.value
end

module type Impl = sig
  val of_string : string -> (t, [> parse_error ]) Result.t

  val to_string : t -> string
end

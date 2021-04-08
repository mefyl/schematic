type parse_error = string

type t = Ezjsonm.value

module type S = sig
  open Base

  val decode : 'a Schema.t -> t -> ('a, Coding.error) Result.t

  val encode : 'a Schema.t -> 'a -> t

  val from_channel : Stdio.In_channel.t -> (t, parse_error) Result.t

  val to_channel : Stdio.Out_channel.t -> t -> unit

  val of_string : string -> (t, parse_error) Result.t

  val to_string : t -> string

  val pp : Formatter.t -> t -> unit

  val to_ezjsonm : t -> Ezjsonm.value
end

module type Impl = sig
  val of_string : string -> (t, parse_error) Result.t

  val to_string : t -> string
end

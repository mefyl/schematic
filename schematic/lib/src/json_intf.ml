type t = Ezjsonm.value

module type S = sig
  val decode : 'a Schema.t -> t -> ('a, Error.decoding) Base.Result.t

  val encode : 'a Schema.t -> 'a -> t

  val from_channel : Stdio.In_channel.t -> (t, Error.parsing) Base.Result.t

  val to_channel : Stdio.Out_channel.t -> t -> unit

  val of_string : string -> (t, Error.parsing) Base.Result.t

  val to_string : t -> string

  val equal : t -> t -> bool

  val pp : Base.Formatter.t -> t -> unit

  val to_ezjsonm : t -> Ezjsonm.value
end

module type Impl = sig
  val of_string : string -> (t, Error.parsing) Result.t

  val to_string : t -> string
end

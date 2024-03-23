type t = Ezjsonm.value
type Coding.t += Coding

module type S = sig
  include Coding.STR with type t = t

  val from_channel : Stdlib.in_channel -> (t, Error.parsing) Base.Result.t
  val to_channel : Stdlib.out_channel -> t -> unit
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Base.Formatter.t -> t -> unit
  val to_ezjsonm : t -> Ezjsonm.value
end

module type Impl = sig
  val of_string : string -> (t, Error.parsing) Result.t
  val to_string : t -> string
end

module Input : sig
  type t

  val of_channel : in_channel -> t
  val of_string : string -> t
end

module Output : sig
  type t

  val of_channel : out_channel -> t
  val of_buffer : Buffer.t -> t
end

include Streaming.S with module Input := Input and module Output := Output

(** Serialize OCaml value in an efficient binary format.

    The format favors streaming speed over space efficiency. It is not
    compatible across machines or versions. *)

module Input : sig
  (** An input channel to read OCaml value from. *)
  type t

  (** [of_channel c] is a [t] that reads from channel [c]. *)
  val of_channel : in_channel -> t

  (** [of_string s] is a [t] that reads from string [s]. *)
  val of_string : string -> t
end

module Output : sig
  (** An output channel to write OCaml value to. *)
  type t

  (** [of_channel c] is a [t] that writes to channel [c]. *)
  val of_channel : out_channel -> t

  (** [of_buffer b] is a [t] that writes to buffer [b]. *)
  val of_buffer : Buffer.t -> t
end

include
  Streaming.S
    with module Input := Input
     and module Output := Output
     and type error := string

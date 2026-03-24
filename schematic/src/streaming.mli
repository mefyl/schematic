include module type of Streaming_intf
module Make (_ : IMPL) : S with type error = Error.Decoding.t

include module type of Streaming_intf
module Make (Impl : IMPL) : S with type error = Error.Decoding.t

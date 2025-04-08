include module type of Streaming_intf

module Make (Impl : IMPL) :
  S with module Input = Impl.Input and module Output = Impl.Output

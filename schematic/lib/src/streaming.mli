include module type of Streaming_intf

module Reader : sig
  include module type of Reader

  include
    Base.Monad.S3 with type ('a, 'state, 'error) t := ('a, 'state, 'error) t

  val fail : 'error -> ('a, 'reader, 'error) t

  val ( let* ) :
    ('a, 'reader, 'error) t ->
    ('a -> ('b, 'reader, 'error) t) ->
    ('b, 'reader, 'error) t

  val ( let+ ) :
    ('a, 'reader, 'error) t -> ('a -> 'b) -> ('b, 'reader, 'error) t
end

module Make (Impl : IMPL) :
  S with type input := Impl.Input.t and type output := Impl.Output.t

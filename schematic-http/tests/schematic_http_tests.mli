module Make
    (Future : Nofuture.S)
    (Handler : Schematic_http.HANDLER with module Future = Future) : sig
  val run :
    ([> float Eio.Time.clock_ty ] Eio.Resource.t ->
    Eio_mock.Clock.Mono.t ->
    [ `Generic | `Unix ] Eio.Net.ty Eio.Net.t ->
    (string * 'a list) list ->
    'b) ->
    (string -> [> `Quick ] -> (Handler.t -> unit Future.t) -> 'a) ->
    'b
end

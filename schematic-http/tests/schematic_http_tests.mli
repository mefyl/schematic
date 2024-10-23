module Make
    (Future : Nofuture.S_IO_TIME)
    (Handler : Schematic_http.HANDLER with module Future = Future) : sig
  val run :
    < net : ([> [> `Generic ] Eio.Net.ty ] as 'a) Eio.Resource.t ; .. > ->
    Eio_mock.Clock.Mono.t ->
    ([> float Eio.Time.clock_ty ] Eio.Resource.t ->
    Eio_mock.Clock.Mono.t ->
    'a Eio.Resource.t ->
    (string * 'b list) list ->
    'c) ->
    (string -> [> `Quick ] -> (Handler.t -> unit Future.t) -> 'b) ->
    'c
end

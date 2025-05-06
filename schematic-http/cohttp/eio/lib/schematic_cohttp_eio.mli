type t

val make :
  ?https:
    (Uri.t ->
    [ `Generic ] Eio.Net.stream_socket_ty Eio.Std.r ->
    [> Eio.Flow.two_way_ty ] Eio.Flow.two_way) ->
  clock:[> float Eio.Time.clock_ty ] Eio.Time.clock ->
  mono_clock:[> Eio.Time.Mono.ty ] Eio.Time.Mono.t ->
  net:[> `Generic ] Eio.Net.ty Eio.Net.t ->
  unit ->
  t

module Make
    (Future : sig
      include Nofuture_eio.S
      include Nofuture.TIME with module Blocking = Blocking
    end)
    (_ : Schematic.Coding.STR with type t = string) :
  Schematic_http.HANDLER with type t = t and module Future = Future

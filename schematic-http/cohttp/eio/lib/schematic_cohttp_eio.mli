type t

val make :
  clock:[> float Eio.Time.clock_ty ] Eio.Time.clock ->
  mono_clock:[> Eio.Time.Mono.ty ] Eio.Time.Mono.t ->
  net:[> `Generic ] Eio.Net.ty Eio.Net.t ->
  t

module Make (Coding : Schematic.Coding.STR with type t = string) :
  Schematic_http.HANDLER
    with type t = t
     and type 'a result = 'a
     and type error = Schematic_http.Error.never
     and module Future = Nofuture_eio

type t

val make : unit -> t

module Make
    (Future : sig
      include Nofuture_lwt.S with type 'a Blocking.t = 'a Lwt.t
      include Nofuture.TIME with module Blocking = Blocking
    end)
    (_ : Cohttp_lwt.S.Client)
    (_ : Schematic.Coding.STR with type t = string) :
  Schematic_http.HANDLER with type t = t and module Future = Future

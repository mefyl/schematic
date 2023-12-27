type t

val make : unit -> t

module Make
    (Client : Cohttp_lwt.S.Client)
    (Coding : Schematic.Coding.STR with type t = string) :
  Schematic_http.HANDLER
    with type t = t
     and type 'a result = 'a
     and type error = Schematic_http.Error.never
     and module Future = Nofuture_lwt

type t

val make : unit -> t

module Make
    (Client : Cohttp_lwt.S.Client)
    (Coding : Schematic.Coding.STR with type t = string) :
  Schematic_http.HANDLER with type t = t and module Future = Nofuture_lwt

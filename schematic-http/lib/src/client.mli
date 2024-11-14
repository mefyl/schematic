include module type of Client_intf
module Make (Handler : HANDLER) : S with module Handler := Handler

val src : Logs.Src.t

module Private : sig
  val error_of_exception : Id.t option -> Uri.t -> exn -> [> `Networking ]
end

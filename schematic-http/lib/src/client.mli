include module type of Client_intf
module Make (Handler : HANDLER) : S with module Handler := Handler

(* (\** [With_timeout (Timeout) (Handler)] is [Handler] with timeout defaulting
   to *)
(*     [Timeout.v]. *\) *)
(* module With_timeout *)
(*     (Timeout : sig *)
(*       val v : Timmy.Span.t *)
(*     end) *)
(*     (Handler : HANDLER) : *)
(*   HANDLER *)
(*     with type t = Handler.t *)
(*      and type 'a result = 'a Handler.result *)
(*      and type error = Handler.error *)
(*      and module Future = Handler.Future *)

(* module With_headers *)
(*     (Headers : sig *)
(*       val headers : (string * string) list *)
(*     end) *)
(*     (Handler : HANDLER) : *)
(*   HANDLER *)
(*     with type t = Handler.t *)
(*      and type 'a result = 'a Handler.result *)
(*      and type error = Handler.error *)
(*      and module Future = Handler.Future *)

val src : Logs.Src.t

module Private : sig
  val error_of_exception :
    Id.t option -> Http.Method.t -> Uri.t -> exn -> [> `Networking ]
end

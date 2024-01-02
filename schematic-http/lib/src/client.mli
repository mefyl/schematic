include module type of Client_intf

module Make (Handler : HANDLER) :
  S
    with type t = Handler.t
     and type 'a result = 'a Handler.result
     and type error = Handler.error
     and module Future = Handler.Future

module Rate_limit
    (Rate : RATE_LIMIT)
    (Handler : HANDLER with module Future = Rate.Future) :
  HANDLER
    with type t = Handler.t
     and type 'a result = 'a Handler.result
     and type error = Handler.error
     and module Future = Handler.Future

module Rate_limit'
    (Rate : RATE_LIMIT')
    (Handler : HANDLER with module Future = Rate.Future) :
  HANDLER
    with type t = Handler.t
     and type 'a result = 'a Handler.result
     and type error = Handler.error

module Optional (Handler : HANDLER) :
  HANDLER
    with type t = Handler.t
     and type 'a result = 'a option Handler.result
     and type error = Handler.error
     and module Future = Handler.Future

module Check_status (Status : STATUS) (Handler : HANDLER) :
  HANDLER
    with type t = Handler.t
     and type 'a result = 'a Handler.result
     and type error = (Status.error, Handler.error) Error.status
     and module Future = Handler.Future

module Status_success : STATUS with type error = Http.Status.t

val check_status_is :
  Http.Status.t -> (module STATUS with type error = Http.Status.t)

(** [With_timeout (Timeout) (Handler)] is [Handler] with timeout defaulting to
    [Timeout.v]. *)
module With_timeout
    (Timeout : sig
      val v : Timmy.Span.t
    end)
    (Handler : HANDLER) :
  HANDLER
    with type t = Handler.t
     and type 'a result = 'a Handler.result
     and type error = Handler.error
     and module Future = Handler.Future

module With_headers
    (Headers : sig
      val headers : (string * string) list
    end)
    (Handler : HANDLER) :
  HANDLER
    with type t = Handler.t
     and type 'a result = 'a Handler.result
     and type error = Handler.error
     and module Future = Handler.Future

module Map_error
    (Handler : HANDLER)
    (E : sig
      type t

      val map : Handler.error -> t
    end) :
  HANDLER
    with type t = Handler.t
     and type 'a result = 'a Handler.result
     and type error = E.t
     and module Future = Handler.Future

val src : Logs.Src.t

module Private : sig
  val error_of_exception :
    Id.t option -> Http.Method.t -> Uri.t -> exn -> [> `Networking ] Error.t
end

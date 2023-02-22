include module type of Client_intf

module Make (Handler : HANDLER) :
  S with type 'a t = 'a Handler.response and type error = Handler.error

module Make'
    (Handler : HANDLER) (Error : sig
      type t

      val map : Handler.error -> t
    end) : S with type 'a t = 'a Handler.response and type error = Error.t

module Rate_limit (Rate : RATE_LIMIT) (Handler : HANDLER) :
  HANDLER
    with type 'a response = 'a Handler.response
     and type error = Handler.error

module Rate_limit' (Rate : RATE_LIMIT') (Handler : HANDLER) :
  HANDLER
    with type 'a response = 'a Handler.response
     and type error = Handler.error

module Optional (Handler : HANDLER) :
  HANDLER
    with type 'a response = 'a option Handler.response
     and type error = Handler.error

module Check_status (Status : STATUS) (Handler : HANDLER) :
  HANDLER
    with type 'a response = 'a Handler.response
     and type error = (Status.error, Handler.error) Error.status

module Status_success : STATUS with type error = Cohttp.Code.status_code

val check_status_is :
  Cohttp.Code.status_code ->
  (module STATUS with type error = Cohttp.Code.status_code)

module With_headers (Headers : sig
  val headers : (string * string) list
end)
(Handler : HANDLER) :
  HANDLER
    with type 'a response = 'a Handler.response
     and type error = Handler.error

module Cohttp
    (Client : Cohttp_lwt.S.Client)
    (Coding : Schematic.Coding.STR with type t = string) :
  HANDLER with type 'a response = 'a and type error = Error.never

val src : Logs.Src.t

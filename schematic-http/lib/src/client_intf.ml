module type S = sig
  type 'a t
  type error

  val raw :
    ?headers:(string * string) list ->
    ?body:'body Schematic.schema * 'body ->
    Cohttp.Code.meth ->
    Uri.t ->
    ((Cohttp.Response.t * Cohttp_lwt.Body.t) t, error Error.t) Lwt_result.t

  val call :
    ?headers:(string * string) list ->
    ?body:'body Schematic.schema * 'body ->
    Cohttp.Code.meth ->
    Uri.t ->
    'response Schematic.schema ->
    ('response t, error Error.t) Lwt_result.t
end

module type RATE_LIMIT = sig
  val backoff : int -> int
  val sleep : float -> unit Lwt.t
end

module type STATUS = sig
  type error

  val check : Cohttp.Code.status_code -> error option
end

module type HANDLER = sig
  type 'a response
  type error

  module Coding : Schematic.Coding.S with type t = string

  module Monad :
    Base.Monad.S2 with type ('a, 'error) t = ('a response, 'error) Lwt_result.t

  val raw :
    ?headers:(string * string) list ->
    ?body:'body Schematic.schema * 'body ->
    Cohttp.Code.meth ->
    Uri.t ->
    ( (Cohttp.Response.t * Cohttp_lwt.Body.t) response,
      error Error.t )
    Lwt_result.t
end

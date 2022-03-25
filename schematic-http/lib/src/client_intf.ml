module type S = sig
  type 'a t

  val raw :
    ?headers:(string * string) list ->
    ?body:'body Schematic.schema * 'body ->
    Cohttp.Code.meth ->
    Uri.t ->
    ((Cohttp.Response.t * Cohttp_lwt.Body.t) t, [> Error.t ]) Lwt_result.t

  val call :
    ?headers:(string * string) list ->
    ?body:'body Schematic.schema * 'body ->
    Cohttp.Code.meth ->
    Uri.t ->
    'response Schematic.schema ->
    ('response t, [> Error.t ]) Lwt_result.t
end

module type RATE_LIMIT = sig
  val backoff : int -> int
  val sleep : float -> unit Lwt.t
end

module type HANDLER = sig
  type 'a response

  module Coding : Schematic.Coding.S with type t = string

  module Monad :
    Base.Monad.S2 with type ('a, 'error) t = ('a response, 'error) Lwt_result.t

  val raw :
    ?headers:(string * string) list ->
    ?body:'body Schematic.schema * 'body ->
    Cohttp.Code.meth ->
    Uri.t ->
    ( (Cohttp.Response.t * Cohttp_lwt.Body.t) response,
      [> Error.t ] )
    Lwt_result.t
end

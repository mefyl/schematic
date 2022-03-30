module type BODY = sig
  type t

  val empty : t
  val is_empty : t -> bool Lwt.t
  val to_string : t -> string Lwt.t
  val to_string_list : t -> string list Lwt.t
  val to_stream : t -> string Lwt_stream.t
  val of_string : string -> t
  val of_string_list : string list -> t
  val of_stream : string Lwt_stream.t -> t
end

type ('a, 'body, 'response) request =
  ?headers:(string * string) list ->
  ?body:'body Schematic.schema * 'body ->
  'response Schematic.schema ->
  'a

module type S = sig
  type 'a t
  type error

  module Body : BODY

  val empty : unit Schematic.Schema.t
  val stream : string Lwt_stream.t Schematic.Schema.t
  val string : string Schematic.Schema.t

  val call :
    ( Cohttp.Code.meth ->
      Uri.t ->
      ('response t, [> error Error.reason ] Error.t) Lwt_result.t,
      'body,
      'response )
    request

  val delete :
    ( Uri.t -> ('response t, [> error Error.reason ] Error.t) Lwt_result.t,
      'body,
      'response )
    request

  val get :
    ( Uri.t -> ('response t, [> error Error.reason ] Error.t) Lwt_result.t,
      'body,
      'response )
    request

  val head :
    ( Uri.t -> ('response t, [> error Error.reason ] Error.t) Lwt_result.t,
      'body,
      'response )
    request

  val patch :
    ( Uri.t -> ('response t, [> error Error.reason ] Error.t) Lwt_result.t,
      'body,
      'response )
    request

  val post :
    ( Uri.t -> ('response t, [> error Error.reason ] Error.t) Lwt_result.t,
      'body,
      'response )
    request

  val put :
    ( Uri.t -> ('response t, [> error Error.reason ] Error.t) Lwt_result.t,
      'body,
      'response )
    request
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

  module Body : BODY
  module Coding : Schematic.Coding.STR with type t = string

  module Monad :
    Base.Monad.S2 with type ('a, 'error) t = ('a response, 'error) Lwt_result.t

  val raw :
    ?headers:(string * string) list ->
    ?body:Body.t ->
    Cohttp.Code.meth ->
    Uri.t ->
    ( (Cohttp.Response.t * Body.t) response,
      [> error Error.reason ] Error.t )
    Lwt_result.t
end

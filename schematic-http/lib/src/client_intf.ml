module type BODY = sig
  type t

  val drain : t -> unit Lwt.t
  val to_string : t -> (string, [> `Networking ] Error.t) Result.t Lwt.t
  val to_stream : t -> (string, [> `Networking ] Error.t) Result.t Lwt_stream.t
  val of_string : Cohttp.Code.meth -> Uri.t -> string -> t
  val of_stream : Cohttp.Code.meth -> Uri.t -> string Lwt_stream.t -> t
end

type 'a response = {
  body : 'a;
  headers : (string * string) list;
  status : Cohttp.Code.status_code;
}

type ('a, 'body, 'response) request =
  ?headers:(string * string) list ->
  ?body:'body Schematic.Schema.t * 'body ->
  'response Schematic.Schema.t ->
  'a

module type REQUESTS = sig
  type 'a res
  type error

  val call :
    ( Cohttp.Code.meth ->
      Uri.t ->
      ('response res, [> error Error.reason ] Error.t) Lwt_result.t,
      'body,
      'response )
    request

  val delete :
    ( Uri.t -> ('response res, [> error Error.reason ] Error.t) Lwt_result.t,
      'body,
      'response )
    request

  val get :
    ( Uri.t -> ('response res, [> error Error.reason ] Error.t) Lwt_result.t,
      'body,
      'response )
    request

  val head :
    ( Uri.t -> ('response res, [> error Error.reason ] Error.t) Lwt_result.t,
      'body,
      'response )
    request

  val patch :
    ( Uri.t -> ('response res, [> error Error.reason ] Error.t) Lwt_result.t,
      'body,
      'response )
    request

  val post :
    ( Uri.t -> ('response res, [> error Error.reason ] Error.t) Lwt_result.t,
      'body,
      'response )
    request

  val put :
    ( Uri.t -> ('response res, [> error Error.reason ] Error.t) Lwt_result.t,
      'body,
      'response )
    request
end

module type S = sig
  type 'a t
  type error

  module Body : BODY

  val empty : unit Schematic.Schema.t
  val string : string Schematic.Schema.t
  val stream : string Lwt_stream.t Schematic.Schema.t

  module Response : sig
    val stream :
      (string, [ `Networking ] Error.t) result Lwt_stream.t Schematic.Schema.t

    include REQUESTS with type 'a res := 'a response t and type error := error
  end

  include REQUESTS with type 'a res := 'a t and type error := error
end

module type RATE_LIMIT = sig
  val backoff : int -> int
  val sleep : float -> unit Lwt.t
end

module type RATE_LIMIT' = sig
  include RATE_LIMIT

  val rate_limited : Cohttp.Response.t -> bool
end

type 'error body_decoder =
  | Body_decoder : {
      schema : 'e Schematic.Schema.t;
      decode : 'e -> 'error;
    }
      -> 'error body_decoder

module type STATUS = sig
  type error

  val check :
    Cohttp.Code.status_code ->
    [ `Body_error of error Schematic.Schema.t
    | `Body_decoder of error body_decoder
    | `Error of error
    | `Ok
    ]
end

module type HANDLER = sig
  type 'a response
  type error

  module Body : BODY
  module Coding : Schematic.Coding.STR with type t = string

  module Monad :
    Base.Monad.S2 with type ('a, 'error) t = ('a response, 'error) Lwt_result.t

  (** Whether to send content length headers.

      Browser XHR automatically fills content-length for security reasons and
      will yield an annoying warning on each request if it is set manually. *)
  val send_content_length : bool

  val raw :
    ?headers:(string * string) list ->
    ?body:Body.t ->
    Cohttp.Code.meth ->
    Uri.t ->
    ( (Cohttp.Response.t * Body.t) response,
      [> error Error.reason ] Error.t )
    Lwt_result.t
end

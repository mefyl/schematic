module type BODY = sig
  type t

  module Future : Nofuture.S

  val drain : t -> unit Future.t
  val to_string : t -> (string, [> `Networking ] Error.t) Result.t Future.t
  val to_channel : t -> Future.In_channel.t
  val of_string : Cohttp.Code.meth -> Uri.t -> string -> t
  val of_channel : Cohttp.Code.meth -> Uri.t -> Future.In_channel.t -> t
end

type 'a response = {
  body : 'a;
  headers : (string * string) list;
  status : Cohttp.Code.status_code;
}

type ('client, 'a, 'body, 'response) request =
  'client ->
  ?timeout:Timmy.Span.t ->
  ?headers:(string * string) list ->
  ?body:'body Schematic.Schema.t * 'body ->
  'response Schematic.Schema.t ->
  'a

module type REQUESTS = sig
  type t
  type 'a res
  type error

  module Future : Nofuture.S

  val call :
    ( t,
      Cohttp.Code.meth ->
      Uri.t ->
      ('response res, [> error Error.reason ] Error.t) Result.t Future.t,
      'body,
      'response )
    request

  val delete :
    ( t,
      Uri.t ->
      ('response res, [> error Error.reason ] Error.t) Result.t Future.t,
      'body,
      'response )
    request

  val get :
    ( t,
      Uri.t ->
      ('response res, [> error Error.reason ] Error.t) Result.t Future.t,
      'body,
      'response )
    request

  val head :
    ( t,
      Uri.t ->
      ('response res, [> error Error.reason ] Error.t) Result.t Future.t,
      'body,
      'response )
    request

  val patch :
    ( t,
      Uri.t ->
      ('response res, [> error Error.reason ] Error.t) Result.t Future.t,
      'body,
      'response )
    request

  val post :
    ( t,
      Uri.t ->
      ('response res, [> error Error.reason ] Error.t) Result.t Future.t,
      'body,
      'response )
    request

  val put :
    ( t,
      Uri.t ->
      ('response res, [> error Error.reason ] Error.t) Result.t Future.t,
      'body,
      'response )
    request
end

module type S = sig
  type t
  type 'a result
  type error

  module Future : Nofuture.S
  module Body : BODY with module Future := Future

  val empty : unit Schematic.Schema.t
  val string : string Schematic.Schema.t
  val stream : Future.In_channel.t Schematic.Schema.t

  module Response : sig
    include
      REQUESTS
        with type t := t
         and type 'a res := 'a response result
         and type error := error
         and module Future := Future
  end

  include
    REQUESTS
      with type t := t
       and type 'a res := 'a result
       and type error := error
       and module Future := Future
end

module type RATE_LIMIT = sig
  module Future : Nofuture.S

  val backoff : int -> int
  val sleep : float -> unit Future.t
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
  type t
  type 'a result
  type error

  module Future : Nofuture.S
  module Body : BODY with module Future := Future
  module Coding : Schematic.Coding.STR with type t = string

  module Monad :
    Base.Monad.S2
      with type ('a, 'error) t = ('a result, 'error) Result.t Future.t

  (** Whether to send content length headers.

      Browser XHR automatically fills content-length for security reasons and
      will yield an annoying warning on each request if it is set manually. *)
  val send_content_length : bool

  val raw :
    t ->
    scope:Future.Scope.t ->
    ?timeout:Timmy.Span.t ->
    ?headers:(string * string) list ->
    ?body:Body.t ->
    Cohttp.Code.meth ->
    Uri.t ->
    ( (Cohttp.Response.t * Body.t) result,
      [> error Error.reason ] Error.t )
    Result.t
    Future.t
end

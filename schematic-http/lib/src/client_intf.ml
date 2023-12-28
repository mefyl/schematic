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

module type REQUESTS = sig
  type ('a, 'body, 'response) t
  type 'a result
  type error

  module Future : Nofuture.S

  val call :
    ( Cohttp.Code.meth ->
      Uri.t ->
      ('response result, [> error Error.reason ] Error.t) Result.t Future.t,
      'body,
      'response )
    t

  val delete :
    ( Uri.t ->
      ('response result, [> error Error.reason ] Error.t) Result.t Future.t,
      'body,
      'response )
    t

  val get :
    ( Uri.t ->
      ('response result, [> error Error.reason ] Error.t) Result.t Future.t,
      'body,
      'response )
    t

  val head :
    ( Uri.t ->
      ('response result, [> error Error.reason ] Error.t) Result.t Future.t,
      'body,
      'response )
    t

  val patch :
    ( Uri.t ->
      ('response result, [> error Error.reason ] Error.t) Result.t Future.t,
      'body,
      'response )
    t

  val post :
    ( Uri.t ->
      ('response result, [> error Error.reason ] Error.t) Result.t Future.t,
      'body,
      'response )
    t

  val put :
    ( Uri.t ->
      ('response result, [> error Error.reason ] Error.t) Result.t Future.t,
      'body,
      'response )
    t
end

module type S = sig
  type t
  type 'a result
  type error

  module Future : Nofuture.S
  module Body : BODY with module Future := Future

  type 'a body =
    | Empty : unit body
    | Channel : Future.Scope.t -> Future.In_channel.t body
    | Schema : 'a Schematic.Schema.t -> 'a body
    | String : string body

  module Response : sig
    include
      REQUESTS
        with type ('a, 'body, 'response) t :=
          t ->
          ?timeout:Timmy.Span.t ->
          ?headers:(string * string) list ->
          ?body:'body body * 'body ->
          'response body ->
          'a
         and type 'a result := 'a response result
         and type error := error
         and module Future := Future
  end

  include
    REQUESTS
      with type ('a, 'body, 'response) t :=
        t ->
        ?timeout:Timmy.Span.t ->
        ?headers:(string * string) list ->
        ?body:'body body * 'body ->
        'response body ->
        'a
       and type 'a result := 'a result
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

type 'a response = {
  body : 'a;
  headers : (string * string) list;
  status : Http.Status.t;
}

type exn += Channel of [ `Networking ] Error.t

module type REQUESTS = sig
  type ('a, 'body, 'response, 'result, 'error) t

  module Future : Nofuture.S

  val call :
    ( Http.Method.t ->
      Uri.t ->
      ( 'result,
        ([> `Decoding of Schematic.Error.decoding | `Networking ] as 'error)
        Error.t )
      Result.t
      Future.t,
      'body,
      'response,
      'result,
      'error )
    t

  val delete :
    ( Uri.t ->
      ( 'result,
        ([> `Decoding of Schematic.Error.decoding | `Networking ] as 'error)
        Error.t )
      Result.t
      Future.t,
      'body,
      'response,
      'result,
      'error )
    t

  val get :
    ( Uri.t ->
      ( 'result,
        ([> `Decoding of Schematic.Error.decoding | `Networking ] as 'error)
        Error.t )
      Result.t
      Future.t,
      'body,
      'response,
      'result,
      'error )
    t

  val head :
    ( Uri.t ->
      ( 'result,
        ([> `Decoding of Schematic.Error.decoding | `Networking ] as 'error)
        Error.t )
      Result.t
      Future.t,
      'body,
      'response,
      'result,
      'error )
    t

  val patch :
    ( Uri.t ->
      ( 'result,
        ([> `Decoding of Schematic.Error.decoding | `Networking ] as 'error)
        Error.t )
      Result.t
      Future.t,
      'body,
      'response,
      'result,
      'error )
    t

  val post :
    ( Uri.t ->
      ( 'result,
        ([> `Decoding of Schematic.Error.decoding | `Networking ] as 'error)
        Error.t )
      Result.t
      Future.t,
      'body,
      'response,
      'result,
      'error )
    t

  val put :
    ( Uri.t ->
      ( 'result,
        ([> `Decoding of Schematic.Error.decoding | `Networking ] as 'error)
        Error.t )
      Result.t
      Future.t,
      'body,
      'response,
      'result,
      'error )
    t
end

type 'error body_decoder =
  | Body_decoder : {
      schema : 'e Schematic.Schema.t;
      decode : 'e -> 'error;
    }
      -> 'error body_decoder

module type HANDLER = sig
  type t

  module Future : Nofuture.S

  module Body : sig
    type t

    val drain : t -> unit Future.t
    val to_string : t -> (string, [> `Networking ]) Result.t Future.t
    val to_channel : t -> Future.In_channel.t
    val of_string : Http.Method.t -> Uri.t -> string -> t
    val of_channel : Http.Method.t -> Uri.t -> Future.In_channel.t -> t
  end

  module Coding : Schematic.Coding.STR with type t = string

  (** Whether to send content length headers.

      Browser XHR automatically fills content-length for security reasons and
      will yield an annoying warning on each request if it is set manually. *)
  val send_content_length : bool

  val raw :
    t ->
    scope:Future.Scope.t ->
    ?timeout:Timmy.Span.t ->
    headers:Http.Header.t ->
    ?body:Body.t ->
    Id.t ->
    Http.Method.t ->
    Uri.t ->
    (Http.Response.t * Body.t, [> `Networking ]) Result.t Future.t
end

module type S = sig
  type ('value, 'result, 'error) t

  module Handler : HANDLER

  module Protocol : sig
    type ('value, 'next, 'result, 'error) rule

    type ('value, 'next, 'result, 'error) t =
      | [] : ('value, 'value, 'result, 'error) t
      | ( :: ) :
          ('value, 'next, 'result, 'error) rule
          * ('next, 'final, 'result, 'error) t
          -> ('value, 'final, 'result, 'error) t

    val check_status :
      (Http.Status.t ->
      [< `Body_decoder of
         ([> `Decoding of Schematic.Error.decoding | `Networking ] as 'error)
         body_decoder
      | `Body of 'error Schematic.Schema.t
      | `Error of 'error
      | `Ok
      ]) ->
      ('value, 'value, 'result, 'error) rule

    val optional : ('value, 'value option, 'result, 'error) rule

    val rate_limit :
      backoff:(int -> int) ->
      rate_limited:(Http.Response.t -> bool) ->
      sleep:(float -> unit Handler.Future.t) ->
      ('value, 'value, 'result, 'error) rule
  end

  val make :
    ?headers:Http.Header.t ->
    ?timeout:Timmy.Span.t ->
    Handler.t ->
    ('value, 'value, [> Error.reason ]) t

  val make_protocol :
    ?headers:Http.Header.t ->
    ?timeout:Timmy.Span.t ->
    Handler.t ->
    ('value, 'result, 'result, 'error) Protocol.t ->
    ('value, 'result, 'error) t

  val with_protocol :
    ('value, 'result, 'error) t ->
    ('next, 'value, 'result, 'error) Protocol.rule ->
    ('next, 'result, 'error) t

  type 'a body =
    | Empty : unit body
    | Channel : Handler.Future.Scope.t -> Handler.Future.In_channel.t body
    | Schema : 'a Schematic.Schema.t -> 'a body
    | String : string body

  module Response : sig
    include
      REQUESTS
        with type ('a, 'body, 'response, 'result, 'error) t :=
          ('response response, 'result, 'error) t ->
          ?timeout:Timmy.Span.t ->
          ?headers:(string * string) list ->
          ?body:'body body * 'body ->
          'response body ->
          'a
         and module Future := Handler.Future
  end

  include
    REQUESTS
      with type ('a, 'body, 'response, 'result, 'error) t :=
        ('response, 'result, 'error) t ->
        ?timeout:Timmy.Span.t ->
        ?headers:(string * string) list ->
        ?body:'body body * 'body ->
        'response body ->
        'a
       and module Future := Handler.Future
end

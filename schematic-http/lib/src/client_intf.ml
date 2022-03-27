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

module type S = sig
  type 'a t
  type error

  module Body : BODY

  val empty : unit Schematic.Schema.t
  val stream : string Lwt_stream.t Schematic.Schema.t
  val string : string Schematic.Schema.t

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

  module Body : BODY
  module Coding : Schematic.Coding.S with type t = string

  module Monad :
    Base.Monad.S2 with type ('a, 'error) t = ('a response, 'error) Lwt_result.t

  val raw :
    ?headers:(string * string) list ->
    ?body:Body.t ->
    Cohttp.Code.meth ->
    Uri.t ->
    ((Cohttp.Response.t * Body.t) response, error Error.t) Lwt_result.t
end

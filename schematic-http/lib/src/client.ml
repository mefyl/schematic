include Client_intf
open Lwt.Infix

let src = Logs.Src.create ~doc:"schematic HTTP client" "schematic-http"

module Logs = (val Logs_lwt.src_log src)

module Lwt_result = struct
  include Lwt_result

  include Base.Monad.Make2 (struct
    type nonrec ('a, 'e) t = ('a, 'e) t

    let return = return
    let bind v ~f = bind v f
    let map = `Custom (fun v ~f -> map f v)
  end)
end

let ( let* ) = ( >>= )
let ( let+ ) = ( >|= )
let ( and* ) = Lwt.both
let ( let*! ) = Lwt_result.( >>= )
let ( >>=! ) = Lwt_result.( >>= )
let empty = Schematic.Schema.make Ignore
let pp_uid = Fmt.(option @@ fmt "[%x] ")

let with_timeout ?(cancel = false) ~method_ ~f name uid uri timeout v =
  match timeout with
  | None -> v
  | Some timeout -> (
    Lwt.choose [ v >|= Base.Option.return; (timeout >|= fun () -> None) ]
    >>= function
    | Some v ->
      let () = if cancel then Lwt.cancel timeout in
      Lwt.return v
    | None ->
      let+ () =
        Logs.warn (fun m ->
            m "%atimeout %s: %s %a" pp_uid uid name
              Cohttp.Code.(string_of_method method_)
              Uri.pp uri)
      in
      f (Result.Error Error.(make ~method_ ~reason:`Networking uri)))

let stream_schema () =
  Schematic.Schema.make
    (Map
       {
         decode = (fun () -> Result.Ok Lwt_stream.(of_list []));
         encode = (fun _ -> ());
         descriptor = Ignore;
       })

let string = Schematic.Schema.make String

module Make_requests (Call : sig
  type 'a res
  type error

  val call :
    ( Cohttp.Code.meth ->
      Uri.t ->
      ('response res, [> error Error.reason ] Error.t) Lwt_result.t,
      'body,
      'response )
    request
end) =
struct
  include (
    Call :
      module type of Call
        with type 'a res := 'a Call.res
         and type error := Call.error)

  let delete ?timeout ?headers ?body response uri =
    call ?timeout ?headers ?body response `DELETE uri

  let get ?timeout ?headers ?body response uri =
    call ?timeout ?headers ?body response `GET uri

  let head ?timeout ?headers ?body response uri =
    call ?timeout ?headers ?body response `HEAD uri

  let patch ?timeout ?headers ?body response uri =
    call ?timeout ?headers ?body response `PATCH uri

  let post ?timeout ?headers ?body response uri =
    call ?timeout ?headers ?body response `POST uri

  let put ?timeout ?headers ?body response uri =
    call ?timeout ?headers ?body response `PUT uri
end

module Map_error
    (Handler : HANDLER)
    (E : sig
      type t

      val map : Handler.error -> t
    end) :
  HANDLER with type 'a response = 'a Handler.response and type error = E.t =
struct
  include Handler

  type error = E.t

  let raw ?timeout ?headers ?body meth uri =
    Handler.raw ?timeout ?headers ?body meth uri
    |> Lwt_result.map_error (function
         | { Error.reason = `Http reason; _ } as error ->
           { error with reason = `Http (E.map reason) }
         | { Error.reason = `Decoding _ | `Networking; _ } as error -> error)
end

module Make (Handler : HANDLER) :
  S with type 'a t = 'a Handler.response and type error = Handler.error = struct
  include Handler

  type 'a t = 'a response
  type error = Handler.error

  let empty = empty
  let stream = stream_schema ()
  let stream_response = stream_schema ()
  let string = string

  let _call (type body response) ?timeout ?(headers = [])
      ?(body : (body Schematic.Schema.t * body) option)
      (response : response Schematic.Schema.t) method_ uri =
    let body, headers =
      let content_length l headers =
        if Handler.send_content_length then
          ("Content-Length", Int.to_string l) :: headers
        else headers
      in
      match body with
      | Some (schema, s) when Obj.magic schema == stream ->
        (Some Body.(of_stream method_ uri (Obj.magic s)), headers)
      | Some (({ descriptor = String; _ } as schema), s) when schema == string
        ->
        ( Some Body.(of_string method_ uri s),
          content_length (String.length s) headers )
      | Some (schema, value) ->
        let body = Coding.encode schema value in
        ( Some (Body.of_string method_ uri body),
          content_length (String.length body) headers )
      | None -> (
        match method_ with
        | `POST | `PUT | `PATCH -> (None, content_length 0 headers)
        | _ -> (None, headers))
    in
    let headers =
      match body with
      | Some _
        when Option.is_none
               ((List.find_opt (fun (name, _) ->
                     String.(lowercase_ascii name = "content-type")))
                  headers) ->
        ("Content-Type", Handler.Coding.content_type) :: headers
      | _ -> headers
    in
    Handler.Monad.bind (raw ?timeout ~headers ?body method_ uri)
      ~f:(fun (resp, body) ->
        let body =
          match response with
          | { descriptor = Ignore; _ } when response == empty ->
            let* () = Body.drain body in
            Handler.Monad.return (() : response)
          | schema when Obj.magic schema == stream_response ->
            Handler.Monad.return (Obj.magic (Body.to_stream body))
          | { descriptor = String; _ } as schema when schema == string ->
            let*! body = Body.to_string body in
            let* () = Logs.debug (fun m -> m "response body:\n%s" body) in
            Handler.Monad.return body
          | response -> (
            let*! body = Body.to_string body in
            let* () = Logs.debug (fun m -> m "response body:\n%s" body) in
            match Coding.decode response body with
            | Result.Ok body -> Handler.Monad.return body
            | Result.Error e ->
              Lwt_result.fail
                (Error.make ~method_
                   ~status:(Cohttp.Response.status resp)
                   ~reason:(`Decoding e) uri))
        in
        Handler.Monad.map body ~f:(fun body -> (body, resp)))

  module Response = struct
    let stream = stream_response

    include Make_requests (struct
      type nonrec 'a res = 'a Client_intf.response t
      and error = Handler.error

      let call ?timeout ?(headers = []) ?body response method_ uri =
        _call ?timeout ~headers ?body response method_ uri
        |> Handler.Monad.map ~f:(fun (body, resp) ->
               {
                 body;
                 headers = Cohttp.Header.to_list resp.Cohttp.Response.headers;
                 status = resp.status;
               })
    end)
  end

  include Make_requests (struct
    type nonrec 'a res = 'a t
    and error = Handler.error

    let call ?timeout ?(headers = []) ?body response method_ uri =
      _call ?timeout ~headers ?body response method_ uri
      |> Handler.Monad.map ~f:(fun (body, _) -> body)
  end)
end

module Cohttp_ = Cohttp

let uid = ref 0

module Cohttp
    (Client : Cohttp_lwt.S.Client)
    (Coding : Schematic.Coding.STR with type t = string) =
struct
  type 'a response = 'a
  type error = Error.never

  let send_content_length = true

  module Coding = Coding
  module Monad = Lwt_result

  let exn method_ uri = function
    | Unix.Unix_error ((Unix.EBADF as e), _, _)
    | Unix.Unix_error ((Unix.ENOTCONN as e), _, _)
    | Unix.Unix_error ((Unix.EADDRNOTAVAIL (* Seen on iOS *) as e), _, _) ->
      let* () =
        Logs.info (fun m ->
            m "retryable unix exception requesting %a: %s" Uri.pp uri
              (Unix.error_message e))
      in
      Lwt_result.fail
        Error.(make ~method_ ~status:(`Code 0) ~reason:`Networking uri)
    | (e : exn) ->
      (* FIXME: Cohttp has no API guarantees about exception that may escape
         depending on the backend. Unreleased recent version yield [Retry]
         exceptions, our own adds [Name_resolution_failed] on top of it, while
         older version may raise [Failure] or whatnot. Pinning such a
         fundamental libraries is a dependency nightmare, so we temporarily
         consider all exceptions as retryable for maximal compatibility and let
         final projects pin a specific version. *)
      let* () =
        Logs.warn (fun m ->
            m "treating cohttp exception as retryabale: %a" Base.Exn.pp e)
      in
      Lwt_result.fail Error.(make ~method_ ~reason:`Networking uri)
  (* let* () =
   *   Logs.err (fun m ->
   *       m "fatal error requesting %a: exception escaped: %a" Uri.pp uri
   *         Base.Exn.pp e)
   * in
   * raise e *)

  module Body = struct
    type t = {
      cohttp : Cohttp_lwt.Body.t;
      method_ : Cohttp.Code.meth;
      timeout : unit Lwt.t option;
      uid : int option;
      uri : Uri.t;
    }

    let of_string method_ uri s =
      {
        cohttp = Cohttp_lwt.Body.of_string s;
        method_;
        timeout = None;
        uid = None;
        uri;
      }

    let of_stream method_ uri s =
      {
        cohttp = Cohttp_lwt.Body.of_stream s;
        method_;
        timeout = None;
        uid = None;
        uri;
      }

    let to_string { cohttp; method_; timeout; uid; uri } =
      let open Lwt in
      catch
        (fun () ->
          with_timeout ~cancel:true ~method_ ~f:Base.Fn.id "reading body" uid
            uri timeout
          @@ let* body = Cohttp_lwt.Body.to_string cohttp in
             let* () =
               Logs.info (fun m ->
                   m "%a read %d bytes of body" pp_uid uid (String.length body))
             in
             Lwt_result.return body)
        (exn method_ uri)

    let to_stream { cohttp; method_; timeout; uid; uri } =
      let body =
        Cohttp_lwt.Body.to_stream cohttp
        |> Lwt_stream.wrap_exn
        |> Lwt_stream.map_s (function
             | Result.Ok _ as ok -> Lwt.return ok
             | Result.Error e -> exn method_ uri e)
      in
      Lwt_stream.from (fun () ->
          with_timeout ~cancel:true ~method_ ~f:Base.Option.return
            "reading body" uid uri timeout (Lwt_stream.get body)
          >>= function
          | Some _ as chunk -> Lwt.return chunk
          | None ->
            let+ () =
              Logs.info (fun m -> m "%a done streaming body" pp_uid uid)
            in
            None)

    let drain { cohttp; _ } =
      Lwt.catch
        (fun () -> Cohttp_lwt.Body.drain_body cohttp)
        (fun _ -> Lwt.return ())
  end

  let raw ?timeout ?headers ?body method_ uri =
    let () = uid := !uid + 1 in
    let uid = !uid in
    let open Lwt in
    let f () =
      let* () =
        Logs.info (fun m ->
            m "[%x] %s %a" uid Cohttp.Code.(string_of_method method_) Uri.pp uri)
      in
      let* () =
        let f (h, v) =
          Logs.info (fun m -> m "[%x]  %s: %s" uid (Base.String.capitalize h) v)
        in
        Lwt_list.iter_s f (Option.value ~default:[] headers)
      in
      let headers = Option.map Cohttp.Header.of_list headers in
      let request =
        Client.call ?headers
          ?body:(Base.Option.map body ~f:(fun { Body.cohttp; _ } -> cohttp))
          method_ uri
        >|= Base.Result.return
      in
      let timeout =
        match timeout with
        | None -> None
        | Some delay -> Some (Timmy_lwt.sleep delay)
      in
      with_timeout ~method_ ~f:Base.Fn.id "reading headers" (Some uid) uri
        timeout request
      >>=! fun (resp, body) ->
      match Cohttp.Response.status resp with
      | `Code 0 as status ->
        (* CORS or connectivity issue *)
        let* () = Cohttp_lwt.Body.drain_body body in
        Lwt_result.fail Error.(make ~method_ ~status ~reason:`Networking uri)
      | status ->
        let* () =
          Logs.info (fun m ->
              m "[%x] %s" uid @@ Cohttp.Code.string_of_status status)
        in
        Lwt_result.return
          (resp, { Body.cohttp = body; method_; timeout; uid = Some uid; uri })
    in
    Lwt.catch f (exn method_ uri)
end

let pp_retry =
  let open Fmt in
  using (fun i -> if i <= 1 then None else Some i)
  @@ option ~none:nop @@ fmt " (%i retries)"

module Rate_limit' (Rate : RATE_LIMIT') (Handler : HANDLER) = struct
  include Handler

  let retry_after = "retry-after"

  let backoff errors uri resp =
    let log pv v =
      Logs.warn (fun m ->
          m "429 Too Many Requests querying %a%a, backing off %a" Uri.pp uri
            pp_retry errors pv v)
    in
    let default () =
      let v = Rate.backoff errors in
      let* () = log Fmt.(fmt "for %i seconds") v in
      Lwt.return v
    in
    match Cohttp_.Header.get Cohttp_.Response.(headers resp) retry_after with
    | None -> default ()
    | Some value -> (
      (* FIXME: handle dates *)
      try
        let v = Base.Int.of_string value in
        let* () =
          log
            Fmt.(
              fmt "for %i seconds (from "
              ++ const string (str "%S" retry_after)
              ++ const string " header)")
            v
        in
        Lwt.return v
      with _ ->
        let* () =
          Logs.warn (fun m ->
              m "unrecognized %S header value %S" retry_after value)
        in
        default ())

  let raw ?timeout ?headers ?body method_ uri =
    let rec raw errors () =
      Handler.Monad.bind (Handler.raw ?timeout ?headers ?body method_ uri)
        ~f:(fun ((resp, _) as r) ->
          if Rate.rate_limited resp then
            let* backoff = backoff errors uri resp in
            let* () = Rate.sleep (Float.of_int backoff) in
            raw (errors + 1) ()
          else Handler.Monad.return r)
    in
    raw 0 ()
end

module Rate_limit (Rate : RATE_LIMIT) (Handler : HANDLER) =
  Rate_limit'
    (struct
      include Rate

      let rate_limited resp =
        match Cohttp_.Response.status resp with
        | `Service_unavailable | `Too_many_requests -> true
        | _ -> false
    end)
    (Handler)

module Optional (Handler : HANDLER) = struct
  include Handler

  module Monad = struct
    type ('a, 'b) t = ('a option, 'b) Handler.Monad.t

    include Base.Monad.Make2 (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let return v = Handler.Monad.return (Some v)

      let bind v ~f =
        Handler.Monad.bind v ~f:(function
          | None -> Handler.Monad.return None
          | Some v -> f v)

      let map = `Custom (fun v ~f -> Handler.Monad.map v ~f:(Option.map f))
    end)
  end

  type 'a response = 'a option Handler.response

  let raw ?timeout ?headers ?body method_ uri =
    Handler.Monad.bind (Handler.raw ?timeout ?headers ?body method_ uri)
      ~f:(fun ((resp, body) as r) ->
        match Cohttp_.Response.status resp with
        | `Not_found ->
          let* () = Handler.Body.drain body in
          Handler.Monad.return None
        | _ -> Handler.Monad.return (Some r))
end

module Check_status (Status : STATUS) (Handler : HANDLER) = struct
  include Handler

  type 'a response = 'a Handler.response
  type error = (Status.error, Handler.error) Error.status

  let raw ?timeout ?headers ?body method_ uri =
    let f ((resp, body) as r) =
      let status = Cohttp_.Response.status resp in
      let rec extract_error = function
        | `Body_error schema ->
          extract_error
            (`Body_decoder (Body_decoder { schema; decode = (fun e -> e) }))
        | `Body_decoder (Body_decoder { schema; decode }) -> (
          let* () =
            Logs.warn (fun m ->
                m "unexpected status %s querying %a"
                  (Cohttp_.Code.string_of_status status)
                  Uri.pp uri)
          in
          let*! body = Body.to_string body in
          match Handler.Coding.decode schema body with
          | Result.Ok error ->
            Lwt_result.fail
              Error.(
                make ~method_ ~status
                  ~reason:(`Http (Unexpected_status (decode error)))
                  uri)
          | Result.Error e ->
            let e =
              {
                e with
                reason = Fmt.str "while decoding error body: %s" e.reason;
              }
            in
            Lwt_result.fail
              Error.(make ~method_ ~status ~reason:(`Decoding e) uri))
        | `Error error ->
          let* () =
            Logs.warn (fun m ->
                m "unexpected status %s querying %a"
                  (Cohttp_.Code.string_of_status status)
                  Uri.pp uri)
          and* () = Body.drain body in
          Lwt_result.fail
            Error.(
              make ~method_ ~status ~reason:(`Http (Unexpected_status error))
                uri)
        | `Ok -> Monad.return r
      in
      extract_error (Status.check status)
    in

    Handler.raw ?timeout ?headers ?body method_ uri
    |> Lwt_result.map_error (function
         | Error.{ reason = `Http e; _ } as error ->
           { error with reason = `Http Error.(Valid_status e) }
         | { reason = `Decoding _ | `Networking; _ } as error -> error)
    |> Handler.Monad.bind ~f
end

module Status_success = struct
  type error = Cohttp_.Code.status_code

  let check status =
    if Cohttp_.Code.(is_success (code_of_status status)) then `Ok
    else `Error status
end

let check_status_is expected =
  let expected = Cohttp_.Code.code_of_status expected in
  (module struct
    type error = Cohttp_.Code.status_code

    let check status =
      if Cohttp_.Code.code_of_status status = expected then `Ok
      else `Error status
  end : STATUS
    with type error = Cohttp_.Code.status_code)

module With_headers
    (Headers : sig
      val headers : (string * string) list
    end)
    (Handler : HANDLER) =
struct
  include Handler

  let raw ?timeout ?(headers = []) ?body method_ uri =
    let headers = Headers.headers @ headers in
    Handler.raw ?timeout ~headers ?body method_ uri
end

module With_timeout
    (Timeout : sig
      val v : Timmy.Span.t
    end)
    (Handler : HANDLER) =
struct
  include Handler

  let raw ?timeout ?(headers = []) ?body method_ uri =
    Handler.raw
      ~timeout:(Option.value ~default:Timeout.v timeout)
      ~headers ?body method_ uri
end

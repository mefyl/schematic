include Client_intf

let src = Logs.Src.create ~doc:"schematic HTTP client" "schematic-http"

module Logs = (val Logs.src_log src)

module Private = struct
  let error_of_exception id method_ uri = function
    | Unix.Unix_error ((Unix.EBADF as e), _, _)
    | Unix.Unix_error ((Unix.ENOTCONN as e), _, _)
    | Unix.Unix_error ((Unix.EADDRNOTAVAIL (* Seen on iOS *) as e), _, _) ->
      let () =
        Logs.info (fun m ->
            m "%aretryable unix exception requesting %a: %s" (Fmt.option Id.pp)
              id Uri.pp uri (Unix.error_message e))
      in
      Error.(make ~method_ ~status:(`Code 0) ~reason:`Networking uri)
    | (e : exn) ->
      (* FIXME: Cohttp has no API guarantees about exception that may escape
         depending on the backend. Unreleased recent version yield [Retry]
         exceptions, our own adds [Name_resolution_failed] on top of it, while
         older version may raise [Failure] or whatnot. Pinning such a
         fundamental libraries is a dependency nightmare, so we temporarily
         consider all exceptions as retryable for maximal compatibility and let
         final projects pin a specific version. *)
      let () =
        Logs.warn (fun m ->
            m "%atreating cohttp exception as retryabale: %a" (Fmt.option Id.pp)
              id Base.Exn.pp e)
      in
      Error.(make ~method_ ~reason:`Networking uri)
end

module Map_error
    (Handler : HANDLER)
    (E : sig
      type t

      val map : Handler.error -> t
    end) =
struct
  include Handler

  type error = E.t

  let raw t ~scope ?timeout ?headers ?body id meth uri =
    Handler.raw t ~scope ?timeout ?headers ?body id meth uri
    |> Future.map
         ~f:
           (Result.map_error (function
             | { Error.reason = `Http reason; _ } as error ->
               { error with reason = `Http (E.map reason) }
             | { Error.reason = `Decoding _ | `Networking; _ } as error -> error))
end

module Make (Handler : HANDLER) = struct
  include Handler

  let ( let* ) v f = Handler.Future.bind v ~f

  type 'a body =
    | Empty : unit body
    | Channel : Future.Scope.t -> Future.In_channel.t body
    | Schema : 'a Schematic.Schema.t -> 'a body
    | String : string body

  module Make_requests (Call : sig
    type 'a result
    type error

    val call :
      t ->
      ?timeout:Timmy.Span.t ->
      ?headers:(string * string) list ->
      ?body:'body body * 'body ->
      'response body ->
      Http.Method.t ->
      Uri.t ->
      ('response result, [> error Error.reason ] Error.t) Result.t Future.t
  end) =
  struct
    include (
      Call :
        module type of Call
          with type 'a result := 'a Call.result
           and type error := Call.error)

    let delete t ?timeout ?headers ?body response uri =
      call t ?timeout ?headers ?body response `DELETE uri

    let get t ?timeout ?headers ?body response uri =
      call t ?timeout ?headers ?body response `GET uri

    let head t ?timeout ?headers ?body response uri =
      call t ?timeout ?headers ?body response `HEAD uri

    let patch t ?timeout ?headers ?body response uri =
      call t ?timeout ?headers ?body response `PATCH uri

    let post t ?timeout ?headers ?body response uri =
      call t ?timeout ?headers ?body response `POST uri

    let put t ?timeout ?headers ?body response uri =
      call t ?timeout ?headers ?body response `PUT uri
  end

  let _call (type request response) t ?timeout ?(headers = [])
      ?(body : (request body * request) option) (response : response body)
      method_ uri =
    let call scope =
      let body, headers =
        let content_length l headers =
          if Handler.send_content_length then
            ("Content-Length", Int.to_string l) :: headers
          else headers
        in
        match body with
        | Some (Channel _, channel) ->
          (Some Body.(of_channel method_ uri channel), headers)
        | Some (String, s) ->
          ( Some Body.(of_string method_ uri s),
            content_length (String.length s) headers )
        | Some (Schema schema, value) ->
          let body = Coding.encode schema value in
          ( Some (Body.of_string method_ uri body),
            content_length (String.length body) headers )
        | Some (Empty, ()) | None -> (
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
      let id = Id.make () in
      let () =
        Logs.info (fun m ->
            m "%a%s %a" Id.pp id (Http.Method.to_string method_) Uri.pp uri)
      in
      let () =
        let f (h, v) =
          Logs.debug (fun m ->
              m "%a  %s: %s" Id.pp id (Base.String.capitalize h) v)
        in
        List.iter f headers
      in
      Handler.Monad.bind (raw t ~scope ?timeout ~headers ?body id method_ uri)
        ~f:(fun (resp, body) ->
          let () =
            Logs.info (fun m ->
                m "%a%s" Id.pp id @@ Http.Status.to_string resp.status)
          in
          let body =
            match response with
            | Empty ->
              let* () = Body.drain body in
              Handler.Monad.return (() : response)
            | Channel _ -> Handler.Monad.return (Body.to_channel body)
            | String -> (
              let* body = Body.to_string body in
              match body with
              | Result.Error _ as error -> Future.return error
              | Result.Ok body ->
                let () = Logs.debug (fun m -> m "response body:\n%s" body) in
                Handler.Monad.return body)
            | Schema response -> (
              let* body = Body.to_string body in
              match body with
              | Result.Error _ as error -> Future.return error
              | Result.Ok body -> (
                let () = Logs.debug (fun m -> m "response body:\n%s" body) in
                match Coding.decode response body with
                | Result.Ok body -> Handler.Monad.return body
                | Result.Error e ->
                  Future.return
                    (Result.Error
                       (Error.make ~method_
                          ~status:(Http.Response.status resp)
                          ~reason:(`Decoding e) uri))))
          in
          Handler.Monad.map body ~f:(fun body -> (body, resp)))
    in
    match response with
    | Channel scope -> call scope
    | _ -> Future.Scope.run call

  module Response = struct
    include Make_requests (struct
      type nonrec 'a result = 'a Client_intf.response result
      and error = Handler.error

      let call t ?timeout ?(headers = []) ?body response method_ uri =
        _call t ?timeout ~headers ?body response method_ uri
        |> Handler.Monad.map ~f:(fun (body, resp) ->
               {
                 body;
                 headers = Http.Header.to_list resp.Http.Response.headers;
                 status = resp.status;
               })
    end)
  end

  include Make_requests (struct
    type nonrec 'a result = 'a result
    and error = Handler.error

    let call t ?timeout ?(headers = []) ?body response method_ uri =
      _call t ?timeout ~headers ?body response method_ uri
      |> Handler.Monad.map ~f:(fun (body, _) -> body)
  end)
end

let pp_retry =
  let open Fmt in
  using (fun i -> if i <= 1 then None else Some i)
  @@ option ~none:nop @@ fmt " (%i retries)"

module Rate_limit'
    (Rate : RATE_LIMIT')
    (Handler : HANDLER with module Future = Rate.Future) =
struct
  include Handler

  let ( let* ) v f = Future.bind v ~f
  let retry_after = "retry-after"

  let backoff errors uri resp =
    let log pv v =
      Logs.warn (fun m ->
          m "429 Too Many Requests querying %a%a, backing off %a" Uri.pp uri
            pp_retry errors pv v)
    in
    let default () =
      let v = Rate.backoff errors in
      let () = log Fmt.(fmt "for %i seconds") v in
      Future.return v
    in
    match Http.Header.get Http.Response.(headers resp) retry_after with
    | None -> default ()
    | Some value -> (
      (* FIXME: handle dates *)
      try
        let v = Base.Int.of_string value in
        let () =
          log
            Fmt.(
              fmt "for %i seconds (from "
              ++ const string (str "%S" retry_after)
              ++ const string " header)")
            v
        in
        Future.return v
      with _ ->
        let () =
          Logs.warn (fun m ->
              m "unrecognized %S header value %S" retry_after value)
        in
        default ())

  let raw t ~scope ?timeout ?headers ?body id method_ uri =
    let rec raw errors () =
      Handler.Monad.bind
        (Handler.raw t ~scope ?timeout ?headers ?body id method_ uri)
        ~f:(fun ((resp, _) as r) ->
          if Rate.rate_limited resp then
            let* backoff = backoff errors uri resp in
            let* () = Rate.sleep (Float.of_int backoff) in
            raw (errors + 1) ()
          else Handler.Monad.return r)
    in
    raw 0 ()
end

module Rate_limit
    (Rate : RATE_LIMIT)
    (Handler : HANDLER with module Future = Rate.Future) =
  Rate_limit'
    (struct
      include Rate

      let rate_limited resp =
        match Http.Response.status resp with
        | `Service_unavailable | `Too_many_requests -> true
        | _ -> false
    end)
    (Handler)

module Optional (Handler : HANDLER) = struct
  include Handler

  type 'a result = 'a option Handler.result

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

  let raw t ~scope ?timeout ?headers ?body id method_ uri =
    Handler.Monad.map
      (Handler.raw t ~scope ?timeout ?headers ?body id method_ uri)
      ~f:(fun ((resp, _) as r) ->
        match Http.Response.status resp with `Not_found -> None | _ -> Some r)
end

module Check_status (Status : STATUS) (Handler : HANDLER) = struct
  include Handler

  let ( let* ) v f = Handler.Future.bind v ~f

  type 'a result = 'a Handler.result
  type error = (Status.error, Handler.error) Error.status

  let raw t ~scope ?timeout ?headers ?body id method_ uri =
    let f ((resp, body) as r) =
      let status = Http.Response.status resp in
      let rec extract_error = function
        | `Body_error schema ->
          extract_error
            (`Body_decoder (Body_decoder { schema; decode = (fun e -> e) }))
        | `Body_decoder (Body_decoder { schema; decode }) -> (
          let () =
            Logs.warn (fun m ->
                m "unexpected status %s querying %a"
                  (Http.Status.to_string status)
                  Uri.pp uri)
          in
          let* body = Body.to_string body in
          match body with
          | Result.Error _ as error -> Future.return error
          | Result.Ok body -> (
            match Handler.Coding.decode schema body with
            | Result.Ok error ->
              Future.return
                (Result.Error
                   Error.(
                     make ~method_ ~status
                       ~reason:(`Http (Unexpected_status (decode error)))
                       uri))
            | Result.Error e ->
              let e =
                {
                  e with
                  reason = Fmt.str "while decoding error body: %s" e.reason;
                }
              in
              Future.return
                (Result.Error
                   Error.(make ~method_ ~status ~reason:(`Decoding e) uri))))
        | `Error error ->
          let () =
            Logs.warn (fun m ->
                m "unexpected status %s querying %a"
                  (Http.Status.to_string status)
                  Uri.pp uri)
          in
          let* () = Body.drain body in
          Future.return
            (Result.Error
               Error.(
                 make ~method_ ~status ~reason:(`Http (Unexpected_status error))
                   uri))
        | `Ok -> Monad.return r
      in
      extract_error (Status.check status)
    in
    Handler.raw t ~scope ?timeout ?headers ?body id method_ uri
    |> Future.map
         ~f:
           (Result.map_error (function
             | Error.{ reason = `Http e; _ } as error ->
               { error with reason = `Http Error.(Valid_status e) }
             | { reason = `Decoding _ | `Networking; _ } as error -> error))
    |> Handler.Monad.bind ~f
end

module Status_success = struct
  type error = Http.Status.t

  let check status =
    match status with #Http.Status.success -> `Ok | status -> `Error status
end

let check_status_is expected =
  (module struct
    type error = Http.Status.t

    let check status =
      if Http.Status.compare status expected = 0 then `Ok else `Error status
  end : STATUS
    with type error = Http.Status.t)

module With_headers
    (Headers : sig
      val headers : (string * string) list
    end)
    (Handler : HANDLER) =
struct
  include Handler

  let raw t ~scope ?timeout ?(headers = []) ?body method_ uri =
    let headers = Headers.headers @ headers in
    Handler.raw t ~scope ?timeout ~headers ?body method_ uri
end

module With_timeout
    (Timeout : sig
      val v : Timmy.Span.t
    end)
    (Handler : HANDLER) =
struct
  include Handler

  let raw t ~scope ?timeout ?(headers = []) ?body method_ uri =
    Handler.raw t ~scope
      ~timeout:(Option.value ~default:Timeout.v timeout)
      ~headers ?body method_ uri
end

include Client_intf

let src = Logs.Src.create ~doc:"schematic HTTP client" "schematic-http"

module Logs = (val Logs.src_log src)

module Private = struct
  let error_of_exception id uri = function
    | Unix.Unix_error ((Unix.EBADF as e), _, _)
    | Unix.Unix_error ((Unix.ENOTCONN as e), _, _)
    | Unix.Unix_error ((Unix.EADDRNOTAVAIL (* Seen on iOS *) as e), _, _) ->
      let () =
        Logs.info (fun m ->
            m "%aretryable unix exception requesting %a: %s" (Fmt.option Id.pp)
              id Uri.pp uri (Unix.error_message e))
      in
      `Networking
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
      `Networking
end

module Make (Handler : HANDLER) = struct
  include Handler

  let ( let* ) v f = Handler.Future.bind v ~f
  let ( >>= ) = ( let* )

  let ( let*! ) v f =
    Handler.Future.bind v ~f:(function
      | Result.Error _ as error -> Future.return error
      | Result.Ok v -> f v)

  let ( >>=! ) = ( let*! )
  let ( let+ ) v f = Handler.Future.map v ~f
  let ( >>| ) = ( let+ )

  type request = {
    handler : t;
    scope : Future.Scope.t;
    timeout : Timmy.Span.t option;
    headers : Http.Header.t;
    body : Body.t option;
    id : Id.t;
    method' : Http.Method.t;
    url : Uri.t;
  }

  let error_context ?response request reason =
    Error.make ~method_:request.method'
      ?status:(Option.map Http.Response.status response)
      ~reason request.url

  module Protocol = struct
    type ('value, 'result, 'error) protocol = {
      f :
        request ->
        (Http.Response.t * Body.t -> ('value, 'error Error.t) Result.t Future.t) ->
        ('result, 'error Error.t) Result.t Future.t;
    }

    type ('value, 'next, 'result, 'error) rule =
      ('next, 'result, 'error) protocol -> ('value, 'result, 'error) protocol

    let id =
      {
        f =
          (fun ({ handler; scope; timeout; headers; body; id; method'; url } as
                request) f ->
            raw handler ~scope ?timeout ~headers ?body id method' url
            >>| Result.map_error (error_context request)
            >>=! f);
      }

    module Retry = struct
      type nonrec 'error rule = {
        rule : 'value. ('value, 'value, 'value, 'error) rule;
      }
    end

    module Error = struct
      type nonrec 'error rule = {
        rule : 'result 'value. ('value, 'value, 'result, 'error) rule;
      }
    end

    let rewrite rewrite =
      let rule protocol =
        {
          f =
            (fun request f ->
              protocol.f request @@ fun (response, body) ->
              rewrite response body >>= function
              | Result.Ok (response, body) -> f (response, body)
              | Result.Error error ->
                Handler.Future.return
                  (Result.Error (error_context ~response request error)));
        }
      in
      { Error.rule }

    let check_status check =
      let rule protocol =
        {
          f =
            (fun request f ->
              protocol.f request @@ fun (response, body) ->
              let status = Http.Response.status response
              and error_context = error_context ~response request in
              let report () =
                Logs.warn (fun m ->
                    m "unexpected status %a querying %a" Http.Status.pp status
                      Uri.pp request.url)
              in
              let decode_body schema decode =
                let () = report () in
                let*! body =
                  Body.to_string body >>| Result.map_error error_context
                in
                match Handler.Coding.decode schema body with
                | Result.Ok error ->
                  Future.return (Result.Error (error_context (decode error)))
                | Result.Error e ->
                  let e =
                    {
                      e with
                      reason = Fmt.str "while decoding error body: %s" e.reason;
                    }
                  in
                  Future.return (Result.Error (error_context (`Decoding e)))
              in
              match check status with
              | `Ok -> f (response, body)
              | `Error error ->
                let () = report () in
                let* () = Body.drain body in
                Future.return (Result.Error (error_context error))
              | `Body schema ->
                let () = report () in
                decode_body schema Base.Fn.id
              | `Body_decoder (Body_decoder { schema; decode }) ->
                let () = report () in
                decode_body schema decode);
        }
      in
      { Error.rule }

    let optional protocol =
      {
        f =
          (fun request f ->
            protocol.f request @@ fun (response, body) ->
            match Http.Response.status response with
            | `Not_found ->
              let* () = Body.drain body in
              Future.return (Result.Ok None)
            | _ ->
              let*! result = f (response, body) in
              Future.return (Result.Ok (Some result)));
      }

    let rate_limit ?(backoff = fun i -> Base.Int.(min i 6 ** 2)) rate_limited =
      let rule protocol =
        let pp_retry =
          let open Fmt in
          using (fun i -> if i <= 1 then None else Some i)
          @@ option ~none:nop @@ fmt " (%i retries)"
        and retry_after = "retry-after" in
        let backoff errors uri resp =
          let log pv v =
            Logs.warn (fun m ->
                m "429 Too Many Requests querying %a%a, backing off %a" Uri.pp
                  uri pp_retry errors pv v)
          in
          let default () =
            let v = backoff errors in
            let () = log Fmt.(fmt "for %i seconds") v in
            v
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
              v
            with _ ->
              let () =
                Logs.warn (fun m ->
                    m "unrecognized %S header value %S" retry_after value)
              in
              default ())
        in
        let f request f =
          let rec rate_limit errors (response, body) =
            if rate_limited response then
              let backoff = backoff errors request.url response in
              let* () = Future.sleep (Float.of_int backoff) in
              protocol.f request (rate_limit (errors + 1))
            else f (response, body)
          in
          protocol.f request @@ rate_limit 0
        in
        { f }
      in
      { Retry.rule }

    type ('value, 'next, 'result, 'error) t =
      | [] : ('value, 'value, 'result, 'error) t
      | ( :: ) :
          ('value, 'next, 'result, 'error) rule
          * ('next, 'final, 'result, 'error) t
          -> ('value, 'final, 'result, 'error) t

    let rec apply :
          'value 'next 'result 'error.
          ('value, 'next, 'result, 'error) t ->
          ('next, 'result, 'error) protocol ->
          ('value, 'result, 'error) protocol =
     fun (type value next result error)
         (protocols : (value, next, result, error) t)
         (base : (next, result, error) protocol) :
         (value, result, error) protocol ->
      match protocols with
      | [] -> base
      | head :: tail -> head @@ apply tail base
  end

  type ('value, 'result, 'error) t = {
    handler : Handler.t;
    headers : Http.Header.t;
    protocol : ('value, 'result, 'result, 'error) Protocol.t;
    timeout : Timmy.Span.t option;
  }

  module Make (P : sig
    type error
    type 'a t

    val headers : Http.Header.t
    val timeout : Timmy.Span.t option
    val handler : Handler.t
    val protocol : ('value, 'value t, 'value t, error) Protocol.t
  end) =
  struct
    let v =
      {
        handler = P.handler;
        headers = P.headers;
        protocol = P.protocol;
        timeout = P.timeout;
      }
  end

  let make_protocol ?(headers = Http.Header.init ()) ?timeout handler protocol =
    { handler; headers; protocol; timeout }

  let make ?headers ?timeout handler =
    make_protocol ?headers ?timeout handler []

  let with_protocol t rule = { t with protocol = rule :: t.protocol }

  type 'a body =
    | Empty : unit body
    | Channel : Future.Scope.t -> Future.In_channel.t body
    | Schema : 'a Schematic.Schema.t -> 'a body
    | String : string body

  module Make_requests (Call : sig
    type 'a result

    val call :
      ( 'response result,
        'result,
        ([> `Decoding of Schematic.Error.decoding | `Networking ] as 'error) )
      t ->
      ?timeout:Timmy.Span.t ->
      ?headers:(string * string) list ->
      ?body:'body body * 'body ->
      'response body ->
      Http.Method.t ->
      Uri.t ->
      ('result, 'error Error.t) Result.t Future.t
  end) =
  struct
    include (Call : module type of Call with type 'a result := 'a Call.result)

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
      method_ uri result =
    let headers = Http.Header.add_list t.headers headers
    and timeout = Base.Option.first_some timeout t.timeout in
    let call scope =
      let body, headers =
        let content_length l headers =
          if Handler.send_content_length then
            Http.Header.add headers "Content-Length" (Int.to_string l)
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
        if Option.is_some body then
          Http.Header.add_unless_exists headers "content-type"
            Handler.Coding.content_type
        else headers
      in
      let id = Id.make () in
      let () =
        Logs.info (fun m ->
            m "%a%a %a" Id.pp id Http.Method.pp method_ Uri.pp uri)
      in
      let () =
        let f name value =
          Logs.debug (fun m -> m "%a  %s: %s" Id.pp id name value)
        in
        Http.Header.iter f headers
      in
      let request =
        {
          handler = t.handler;
          scope;
          timeout;
          headers;
          body;
          id;
          method' = method_;
          url = uri;
        }
      in
      let protocol = Protocol.apply t.protocol Protocol.id in
      protocol.f request @@ fun (resp, body) ->
      let () =
        Logs.info (fun m ->
            m "%a%s" Id.pp id @@ Http.Status.to_string resp.status)
      and error reason =
        Error.make ~method_ ~status:(Http.Response.status resp) ~reason uri
      in
      let*! body =
        match response with
        | Empty ->
          let* () = Body.drain body in
          Future.return (Result.Ok (() : response))
        | Channel _ -> Future.return (Result.Ok (Body.to_channel body))
        | String -> (
          let* body = Body.to_string body >>| Result.map_error error in

          match body with
          | Result.Error _ as error -> Future.return error
          | Result.Ok body ->
            let () = Logs.debug (fun m -> m "response body:\n%s" body) in
            Future.return (Result.Ok body))
        | Schema response -> (
          let*! body = Body.to_string body >>| Result.map_error error in
          let () = Logs.debug (fun m -> m "response body:\n%s" body) in
          match Coding.decode response body with
          | Result.Ok body -> Future.return (Result.Ok body)
          | Result.Error e -> Future.return (Result.Error (error (`Decoding e)))
          )
      in
      Future.return (Result.Ok (result (body, resp)))
    in
    match response with
    | Channel scope -> call scope
    | _ -> Future.Scope.run call

  module Response = struct
    include Make_requests (struct
      type nonrec 'a result = 'a Client_intf.response

      let call t ?timeout ?(headers = []) ?body response method_ uri =
        _call t ?timeout ~headers ?body response method_ uri
        @@ fun (body, resp) ->
        {
          body;
          headers = Http.Header.to_list resp.Http.Response.headers;
          status = resp.status;
        }
    end)
  end

  include Make_requests (struct
    type nonrec 'a result = 'a

    let call t ?timeout ?(headers = []) ?body response method_ uri =
      _call t ?timeout ~headers ?body response method_ uri @@ fun (body, _) ->
      body
  end)
end

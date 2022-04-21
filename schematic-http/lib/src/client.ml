include Client_intf

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

let ( let* ) = Lwt.bind
let ( and* ) = Lwt.both
let empty = Schematic.Schema.make Ignore

let stream =
  Schematic.Schema.make
    (Map
       {
         decode = (fun () -> Result.Ok Lwt_stream.(of_list []));
         encode = (fun _ -> ());
         descriptor = Ignore;
       })

let string = Schematic.Schema.make String

module Make (Handler : HANDLER) = struct
  include Handler

  type 'a t = 'a response
  type error = Handler.error

  let encode (schema, value) = Coding.encode schema value |> Body.of_string
  let empty = empty
  let stream = stream
  let string = string

  let call (type body response) ?(headers = [])
      ?(body : (body Schematic.Schema.t * body) option)
      (response : response Schematic.Schema.t) method_ uri =
    let body, headers =
      match body with
      | Some (schema, s) when Obj.magic schema == stream ->
        (Some Body.(of_stream (Obj.magic s)), headers)
      | Some (({ descriptor = String; _ } as schema), s) when schema == string
        ->
        ( Some Body.(of_string s),
          ("Content-Length", Int.to_string (String.length s)) :: headers )
      | Some body -> (Some (encode body), headers)
      | None -> (None, headers)
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
    Handler.Monad.bind (raw ~headers ?body method_ uri) ~f:(fun (resp, body) ->
        match response with
        | { descriptor = Ignore; _ } when response == empty ->
          let* () = Body.drain body in
          Handler.Monad.return (() : response)
        | schema when Obj.magic schema == stream ->
          Handler.Monad.return (Obj.magic (Body.to_stream body))
        | { descriptor = String; _ } as schema when schema == string ->
          let* body = Body.to_string body in
          let* () = Logs.debug (fun m -> m "response body:\n%s" body) in
          Handler.Monad.return body
        | response -> (
          let* body = Body.to_string body in
          let* () = Logs.debug (fun m -> m "response body:\n%s" body) in
          match Coding.decode response body with
          | Result.Ok r -> Handler.Monad.return r
          | Result.Error e ->
            Lwt_result.fail
              (Error.make ~method_
                 ~status:(Cohttp.Response.status resp)
                 ~reason:(`Decoding e) uri)))

  let delete ?headers ?body response uri =
    call ?headers ?body response `DELETE uri

  let get ?headers ?body response uri = call ?headers ?body response `GET uri
  let head ?headers ?body response uri = call ?headers ?body response `HEAD uri

  let patch ?headers ?body response uri =
    call ?headers ?body response `PATCH uri

  let post ?headers ?body response uri = call ?headers ?body response `POST uri
  let put ?headers ?body response uri = call ?headers ?body response `PUT uri
end

module Cohttp_ = Cohttp

module Cohttp
    (Client : Cohttp_lwt.S.Client)
    (Coding : Schematic.Coding.STR with type t = string) =
struct
  type 'a response = 'a
  type error = Error.never

  module Coding = Coding
  module Monad = Lwt_result

  module Body = struct
    include Cohttp_lwt.Body

    let drain = drain_body
  end

  let raw ?headers ?body method_ uri =
    let f () =
      let* () =
        Logs.info (fun m ->
            m "%s %a" Cohttp.Code.(string_of_method method_) Uri.pp uri)
      in
      let* () =
        let f (h, v) =
          Logs.info (fun m -> m "  %s: %s" (Base.String.capitalize h) v)
        in
        Lwt_list.iter_s f (Option.value ~default:[] headers)
      in
      let headers = Option.map Cohttp.Header.of_list headers in
      let* resp, body = Client.call ?headers ?body method_ uri in
      match Cohttp.Response.status resp with
      | `Code 0 as status ->
        (* CORS or connectivity issue *)
        Lwt_result.fail Error.(make ~method_ ~status ~reason:`Networking uri)
      | _ -> Lwt_result.return (resp, body)
    and catch (e : exn) =
      let* () =
        Logs.err (fun m ->
            m "fatal error requesting %a: exception escaped: %a" Uri.pp uri
              Base.Exn.pp e)
      in
      raise e
    in
    Lwt.catch f catch
end

module type RATE_LIMIT = sig
  val backoff : int -> int
  val sleep : float -> unit Lwt.t
end

let pp_retry =
  let open Fmt in
  using (fun i -> if i <= 1 then None else Some i)
  @@ option ~none:nop @@ fmt " (%i retries)"

module Rate_limit (Rate : RATE_LIMIT) (Handler : HANDLER) = struct
  include Handler

  let errors = ref 0
  let retry_after = "retry-after"

  let backoff uri resp =
    let log pv v =
      Logs.warn (fun m ->
          m "429 Too Many Requests querying %a%a, backing off %a" Uri.pp uri
            pp_retry !errors pv v)
    in
    let default () =
      let v = Rate.backoff !errors in
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

  let raw ?headers ?body method_ uri =
    let rec raw () =
      Handler.Monad.bind (Handler.raw ?headers ?body method_ uri)
        ~f:(fun ((resp, _) as r) ->
          match Cohttp_.Response.status resp with
          | `Too_many_requests ->
            let () = errors := !errors + 1 in
            let* backoff = backoff uri resp in
            let* () = Rate.sleep (Float.of_int backoff) in
            raw ()
          | _ -> Handler.Monad.return r)
    in
    raw ()
end

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

  let raw ?headers ?body method_ uri =
    Handler.Monad.map (Handler.raw ?headers ?body method_ uri)
      ~f:(fun ((resp, _) as r) ->
        match Cohttp_.Response.status resp with
        | `Not_found -> None
        | _ -> Some r)
end

module Check_status (Status : STATUS) (Handler : HANDLER) = struct
  include Handler

  type 'a response = 'a Handler.response
  type error = (Status.error, Handler.error) Error.status

  let raw ?headers ?body method_ uri =
    let f ((resp, body) as r) =
      let status = Cohttp_.Response.status resp in
      match Status.check status with
      | `Body_error schema -> (
        let* () =
          Logs.warn (fun m ->
              m "unexpected status %s querying %a"
                (Cohttp_.Code.string_of_status status)
                Uri.pp uri)
        and* body = Body.to_string body in
        match Handler.Coding.decode schema body with
        | Result.Ok error ->
          Lwt_result.fail
            Error.(
              make ~method_ ~status ~reason:(`Http (Unexpected_status error))
                uri)
        | Result.Error e ->
          let e =
            { e with reason = Fmt.str "while decoding error body: %s" e.reason }
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
            make ~method_ ~status ~reason:(`Http (Unexpected_status error)) uri)
      | `Ok -> Monad.return r
    in
    Handler.raw ?headers ?body method_ uri
    |> Lwt_result.map_err (function
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

module With_headers (Headers : sig
  val headers : (string * string) list
end)
(Handler : HANDLER) =
struct
  include Handler

  let raw ?(headers = []) ?body method_ uri =
    let headers = Headers.headers @ headers in
    Handler.raw ~headers ?body method_ uri
end

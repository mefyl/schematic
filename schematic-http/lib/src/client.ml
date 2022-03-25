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

module Make (Handler : HANDLER) = struct
  include Handler

  type 'a t = 'a response

  let call ?headers ?body method_ uri response =
    Handler.Monad.bind (raw ?headers ?body method_ uri) ~f:(fun (_, body) ->
        let* body = Cohttp_lwt.Body.to_string body in
        match Coding.decode response body with
        | Result.Ok r -> Handler.Monad.return r
        | Result.Error e -> Lwt_result.fail (`Coding e))
end

module Cohttp_ = Cohttp

module Cohttp
    (Client : Cohttp_lwt.S.Client)
    (Coding : Schematic.Coding.S with type t = string) =
struct
  type 'a response = 'a

  module Coding = Coding
  module Monad = Lwt_result

  let encode (schema, value) =
    Coding.encode schema value |> Cohttp_lwt.Body.of_string

  let raw ?headers ?body method_ uri =
    let body = Option.map encode body
    and headers = Option.map Cohttp.Header.of_list headers in
    let f () =
      let* () =
        Logs.info (fun m ->
            m "%s %a" Cohttp.Code.(string_of_method method_) Uri.pp uri)
      and* resp, body = Client.call ?headers ?body method_ uri in
      match Cohttp.Response.status resp with
      | `Code 0 as status ->
        (* CORS or connectivity issue *)
        Lwt_result.fail @@ `Networking (Error.make_http ~method_ ~status uri)
      | _ -> Lwt_result.return (resp, body)
    and catch (_ : exn) =
      Lwt_result.fail
      @@ `Assert
           (Fmt.str "fatal error requesting %s: %s" (Uri.to_string uri)
              "unkown exception escaped")
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

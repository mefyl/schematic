module Logs = (val Logs.src_log Schematic_http.src)
module Result = Base.Result

let pp_uid = Fmt.(option @@ fmt "[%x] ")
let uid = ref 0

type t = {
  client : Cohttp_eio.Client.t;
  clock : float Eio.Time.clock_ty Eio.Time.clock;
  mono_clock : Eio.Time.Mono.ty Eio.Time.Mono.t;
}

let make ~clock ~mono_clock ~net =
  {
    client = Cohttp_eio.Client.make ~https:None net;
    clock : [> float Eio.Time.clock_ty ] Eio.Time.clock
    :> float Eio.Time.clock_ty Eio.Time.clock;
    mono_clock : [> Eio.Time.Mono.ty ] Eio.Time.Mono.t
    :> Eio.Time.Mono.ty Eio.Time.Mono.t;
  }

module Make (Coding : Schematic.Coding.STR with type t = string) = struct
  type nonrec t = t
  type 'a result = 'a
  type error = Schematic_http.Error.never

  module Future = Nofuture_eio

  let send_content_length = true

  module Coding = Coding
  module Monad = Result

  let error_of_exn method_ uri = function
    | Eio.Time.Timeout ->
      let () = Logs.info (fun m -> m "timeout requesting %a" Uri.pp uri) in
      Schematic_http.Error.(make ~method_ ~reason:`Networking uri)
    | Unix.Unix_error ((Unix.EBADF as e), _, _)
    | Unix.Unix_error ((Unix.ENOTCONN as e), _, _)
    | Unix.Unix_error ((Unix.EADDRNOTAVAIL (* Seen on iOS *) as e), _, _) ->
      let () =
        Logs.info (fun m ->
            m "retryable unix exception requesting %a: %s" Uri.pp uri
              (Unix.error_message e))
      in
      Schematic_http.Error.(
        make ~method_ ~status:(`Code 0) ~reason:`Networking uri)
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
            m "treating cohttp exception as retryabale: %a" Base.Exn.pp e)
      in
      Schematic_http.Error.(make ~method_ ~reason:`Networking uri)

  module Body = struct
    type nonrec t = {
      body : Cohttp_eio.Body.t;
      method_ : Cohttp.Code.meth;
      deadline : (t * float) option;
      uid : int option;
      uri : Uri.t;
    }

    let of_string method_ uri s =
      {
        body = Cohttp_eio.Body.of_string s;
        method_;
        deadline = None;
        uid = None;
        uri;
      }

    let of_channel method_ uri body =
      {
        body : _ Eio.Flow.source :> Eio.Flow.source_ty Eio.Resource.t;
        method_;
        deadline = None;
        uid = None;
        uri;
      }

    let with_deadline deadline f =
      let timeout =
        match deadline with
        | None -> Eio.Time.Timeout.none
        | Some ({ clock; mono_clock; _ }, deadline) ->
          Eio.Time.Timeout.seconds mono_clock @@ (deadline -. Eio.Time.now clock)
      in
      Eio.Time.Timeout.run_exn timeout f

    let to_string { body; method_; deadline; uri; _ } =
      try
        with_deadline deadline @@ fun () ->
        Eio.Flow.read_all body |> Result.return
      with Eio.Time.Timeout as exn ->
        raise (Schematic_http.Channel (error_of_exn method_ uri exn))

    let to_channel { body; deadline; method_; uri; _ } =
      match deadline with
      | None -> body
      | Some _ ->
        let module Source_timeout = struct
          type t = Eio.Flow.source_ty Eio.Resource.t

          let single_read t dst =
            try with_deadline deadline @@ fun () -> Eio.Flow.single_read t dst
            with Eio.Time.Timeout as exn ->
              raise (Schematic_http.Channel (error_of_exn method_ uri exn))

          let read_methods = []
        end in
        let ops = Eio.Flow.Pi.source (module Source_timeout) in
        Eio.Resource.T (body, ops)

    let drain _ =
      (* AFAICT cohttp-eio does not support keepalive and draining so far *) ()
  end

  let raw ({ client; clock; mono_clock } as t) ~scope ?timeout ?headers ?body
      method_ uri =
    let () = uid := !uid + 1 in
    let uid = !uid in
    try
      let () =
        Logs.info (fun m ->
            m "[%x] %s %a" uid Cohttp.Code.(string_of_method method_) Uri.pp uri)
      in
      let () =
        let f (h, v) =
          Logs.debug (fun m ->
              m "[%x]  %s: %s" uid (Base.String.capitalize h) v)
        in
        List.iter f (Option.value ~default:[] headers)
      in
      let headers = Option.map Cohttp.Header.of_list headers in
      let timeout, deadline =
        match timeout with
        | None -> (Eio.Time.Timeout.none, None)
        | Some timeout ->
          let seconds = Timmy.Span.to_seconds timeout |> Float.of_int in
          ( Eio.Time.Timeout.seconds mono_clock seconds,
            Some (t, Eio.Time.now clock +. seconds) )
      in
      Eio.Time.Timeout.run_exn timeout @@ fun () ->
      let resp, body =
        Cohttp_eio.Client.call ~sw:scope ?headers
          ?body:(Base.Option.map body ~f:(fun { Body.body; _ } -> body))
          client method_ uri
      in
      match Cohttp.Response.status resp with
      | `Code 0 as status ->
        (* CORS or connectivity issue *)
        Result.fail
          Schematic_http.Error.(make ~method_ ~status ~reason:`Networking uri)
      | status ->
        let () =
          Logs.info (fun m ->
              m "[%x] %s" uid @@ Cohttp.Code.string_of_status status)
        in
        Result.return
          (resp, { Body.body; method_; deadline; uid = Some uid; uri })
    with exn -> Result.Error (error_of_exn method_ uri exn)
end

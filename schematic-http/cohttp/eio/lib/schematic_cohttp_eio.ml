module Logs = (val Logs.src_log Schematic_http.src)
module Result = Base.Result

type t = {
  client : Cohttp_eio.Client.t;
  clock : float Eio.Time.clock_ty Eio.Time.clock;
  mono_clock : Eio.Time.Mono.ty Eio.Time.Mono.t;
}

let make ?https ~clock ~mono_clock ~net () =
  {
    client = Cohttp_eio.Client.make ~https net;
    clock : [> float Eio.Time.clock_ty ] Eio.Time.clock
    :> float Eio.Time.clock_ty Eio.Time.clock;
    mono_clock : [> Eio.Time.Mono.ty ] Eio.Time.Mono.t
    :> Eio.Time.Mono.ty Eio.Time.Mono.t;
  }

module Make (Coding : Schematic.Coding.STR with type t = string) = struct
  type nonrec t = t

  module Future = Nofuture_eio

  let send_content_length = true

  module Coding = Coding
  module Monad = Result

  let error_of_exn id uri = function
    | Eio.Time.Timeout ->
      let () =
        Logs.info (fun m ->
            m "%atimeout requesting %a"
              (Fmt.option Schematic_http.Id.pp)
              id Uri.pp uri)
      in
      `Networking
    | exn -> Schematic_http.Private.error_of_exception id uri exn

  module Body = struct
    type nonrec t = {
      body : Cohttp_eio.Body.t;
      deadline : (t * float) option;
      id : Schematic_http.Id.t option;
      method_ : Http.Method.t;
      status : Http.Status.t option;
      uri : Uri.t;
    }

    let of_string method_ uri s =
      let (Eio.Resource.T (impl, ops)) = Cohttp_eio.Body.of_string s in
      {
        body =
          Eio.Resource.T
            ( impl,
              Eio.Resource.handler
                (H (Eio.Resource.Close, fun _ -> ())
                :: Eio.Resource.bindings ops) );
        deadline = None;
        id = None;
        method_;
        status = None;
        uri;
      }

    let of_channel method_ uri body =
      {
        body : [> Eio.Flow.source_ty | `Close ] Eio.Flow.source
        :> Cohttp_eio.Body.t;
        deadline = None;
        id = None;
        method_;
        status = None;
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

    let to_string { body; deadline; id; method_; status; uri; _ } =
      try
        with_deadline deadline @@ fun () ->
        Eio.Flow.read_all body |> Result.return
      with Eio.Time.Timeout as exn ->
        raise
          (Schematic_http.Channel
             (Schematic_http.Error.make ~method_
                ~reason:(error_of_exn id uri exn) ?status uri))

    let drain _ =
      (* AFAICT cohttp-eio does not support keepalive and draining so far *) ()

    let to_channel ({ body; deadline; id; method_; status; uri; _ } as t) =
      let body =
        let (Eio.Resource.T (body, ops)) = body in
        Eio.Resource.T
          ( body,
            Eio.Resource.handler
              (H (Eio.Resource.Close, fun _ -> drain t)
              :: Eio.Resource.bindings ops) )
      in
      match deadline with
      | None -> body
      | Some _ ->
        let module Source_timeout = struct
          type t = Future.In_channel.t

          let single_read t dst =
            try with_deadline deadline @@ fun () -> Eio.Flow.single_read t dst
            with Eio.Time.Timeout as exn ->
              raise
                (Schematic_http.Channel
                   (Schematic_http.Error.make ~method_ ?status
                      ~reason:(error_of_exn id uri exn) uri))

          let read_methods = []
        end in
        Eio.Resource.T
          ( body,
            Eio.Resource.handler
              [
                H (Eio.Resource.Close, Eio.Resource.close);
                H (Eio.Flow.Pi.Source, (module Source_timeout));
              ] )
  end

  let raw ({ client; clock; mono_clock } as t) ~scope ?timeout ~headers ?body id
      method_ uri =
    try
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
        Cohttp_eio.Client.call ~sw:scope ~headers
          ?body:
            (Base.Option.map body ~f:(fun { Body.body; _ } ->
                 (body :> Cohttp_eio.Body.t)))
          client method_ uri
      in
      match Cohttp.Response.status resp with
      | `Code 0 ->
        (* CORS or connectivity issue *)
        Result.fail `Networking
      | status ->
        Result.return
          ( resp,
            {
              Body.body;
              deadline;
              id = Some id;
              method_;
              status = Some status;
              uri;
            } )
    with exn -> Result.Error (error_of_exn (Some id) uri exn)
end

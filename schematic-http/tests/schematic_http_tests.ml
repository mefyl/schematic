module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

let () = Logs.set_reporter @@ Logs_fmt.reporter ()
let () = Logs.Src.set_level Schematic_http.src @@ Some Debug
let () = Logs.set_level (Some Debug)

type payload = {
  x : int;
  y : string;
}
[@@deriving eq, schema, show]

let const_payload = { x = 26; y = "105" }
let payload = Alcotest.testable pp_payload equal_payload

let sleep_and_advance ?(flush = false) clock seconds =
  Eio.Switch.run @@ fun sw ->
  let sleep =
    Eio.Fiber.fork_promise ~sw @@ fun () -> Eio.Time.Mono.sleep clock seconds
  in
  let () =
    let rec advance () =
      if Eio_mock.Clock.Mono.try_advance clock && flush then advance ()
    in
    advance ()
  in
  match Eio.Promise.await sleep with
  | Result.Ok () -> ()
  | Result.Error exn -> raise exn

let server mono_clock net sw =
  let server =
    let callback _connection request body =
      let path = Cohttp.Request.uri request |> Uri.path in
      let () = Logs.info (fun m -> m "receive request %S" path) in
      match path with
      | "/big" ->
        let response = Cohttp.Response.make ()
        and body = Cohttp_eio.Body.of_string Data.v in
        (response, body)
      | "/big-slow" ->
        let () = sleep_and_advance mono_clock 2. in
        let response = Cohttp.Response.make ()
        and body =
          let module String_source_chunked = struct
            type t = {
              s : string;
              mutable offset : int;
            }

            let single_read t dst =
              if t.offset = String.length t.s then raise End_of_file;
              let len =
                min 8 @@ min (Cstruct.length dst) (String.length t.s - t.offset)
              in
              let () = Logs.debug (fun m -> m "big-slow: send %d bytes" len) in
              let () = sleep_and_advance mono_clock 0.01 in
              Cstruct.blit_from_string t.s t.offset dst 0 len;
              t.offset <- t.offset + len;
              len

            let read_methods = []
            let create s = { s; offset = 0 }
          end in
          let ops = Eio.Flow.Pi.source (module String_source_chunked) in
          Eio.Resource.T (String_source_chunked.create Data.v, ops)
        in
        (response, body)
      | "/cat" ->
        let response = Cohttp.Response.make () in
        (response, body)
      | "/empty" ->
        let response = Cohttp.Response.make ()
        and body = Cohttp_eio.Body.of_string "" in
        (response, body)
      | "/const" ->
        let response = Cohttp.Response.make ()
        and body =
          Cohttp_eio.Body.of_string
            (Schematic.Json.String.encode payload_schema const_payload)
        in
        (response, body)
      | "/404" ->
        let response = Cohttp.Response.make ~status:`Not_found ()
        and body = Cohttp_eio.Body.of_string "INVALID" in
        (response, body)
      | _ ->
        let response = Cohttp.Response.make ~status:`Not_found ()
        and body = Cohttp_eio.Body.of_string "" in
        (response, body)
    in
    Cohttp_eio.Server.make ~callback ()
  in
  let socket =
    Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let () =
    Eio.Fiber.fork ~sw @@ fun () ->
    let serve () =
      Cohttp_eio.Server.run
        ~on_error:(function
          | exn -> Alcotest.failf "server crashed: %s" @@ Printexc.to_string exn)
        socket server
    in
    serve ()
  in
  match Eio.Net.listening_addr socket with
  | `Tcp (_, port) -> port
  | `Unix _ -> failwith "listen yielded a Unix socket"

type reason =
  [ `Decoding of Schematic.Error.decoding
  | `Unexpected_status of
    (Http.Status.t[@equal fun l r -> Http.Status.compare l r = 0])
  | `Networking
  ]
[@@deriving eq]

let pp_reason fmt = function
  | #Schematic_http.Error.reason as reason ->
    Schematic_http.Error.pp_reason fmt reason
  | `Unexpected_status status ->
    Fmt.pf fmt "unexpected status: %a" Http.Status.pp status

let error =
  Alcotest.testable
    (Schematic_http.Error.pp pp_reason)
    (Schematic_http.Error.equal equal_reason)

module Make
    (Future : Nofuture.S)
    (Handler : Schematic_http.HANDLER with module Future = Future) =
struct
  let ( let* ) v f = Future.bind v ~f
  let ( let+ ) v f = Future.map v ~f

  type server = { port : int }

  module Client = Schematic_http.Make (Handler)

  let uri { port } path =
    Uri.make ~scheme:"http"
      ~host:Eio.Net.Ipaddr.(Fmt.str "%a" pp V4.loopback)
      ~port ~path ()

  module Requests = struct
    let get_empty server handler =
      let client = Client.make handler in
      let+ result = Client.get client Empty @@ uri server "/empty" in
      Alcotest.(check ~here:[%here] (result unit error))
        "request succeded" (Result.Ok ()) result

    let get_json server handler =
      let client = Client.make handler in
      let+ result =
        Client.get client (Schema payload_schema) @@ uri server "/const"
      in
      Alcotest.(check ~here:[%here] (result payload error))
        "request succeded" (Result.Ok const_payload) result

    let get_string server handler =
      let client = Client.make handler in
      let+ result = Client.get client String @@ uri server "/const" in
      Alcotest.(check ~here:[%here] (result string error))
        "request succeded"
        (Result.Ok (Schematic.Json.String.encode payload_schema const_payload))
        result

    let _get_stream server handler path =
      let client = Client.make handler in
      Future.Scope.run ~name:"read stream" @@ fun scope ->
      let* result = Client.get client (Channel scope) @@ uri server path in
      let+ result =
        match result with
        | Result.Error _ as error -> Future.return error
        | Result.Ok stream ->
          let+ content = Future.In_channel.read_all_string stream in
          Result.Ok content
      in
      Alcotest.(check ~here:[%here] (result string error))
        "request succeded" (Result.Ok Data.v) result

    let get_stream server handler = _get_stream server handler "/big"
    let get_stream_slow server handler = _get_stream server handler "/big-slow"

    let post server handler =
      let client = Client.make handler in
      let post_payload = { x = 107; y = "44" } in
      let+ result =
        Client.post client
          ~body:(Schema payload_schema, post_payload)
          (Schema payload_schema)
        @@ uri server "/cat"
      in
      Alcotest.(check ~here:[%here] (result payload error))
        "request succeded" (Result.Ok post_payload) result
  end

  module Protocol = struct
    let status =
      Client.Protocol.check_status (function
        | #Http.Status.success -> `Ok
        | status -> `Error (`Unexpected_status status))

    let status = status.rule

    let not_found server handler =
      let client =
        Client.make_protocol handler Client.Protocol.[ status; optional ]
      in
      let uri = uri server "/404" in
      let+ result = Client.get client (Schema payload_schema) uri in
      Alcotest.(check ~here:[%here] (result (option payload) error))
        "request succeded" (Result.Ok None) result

    let unexpected_status server handler =
      let module C = Client.Make (struct
        type error =
          [ `Decoding of Schematic.Error.decoding
          | `Networking
          | `Unexpected_status of Http.Status.t
          ]

        type 'a t = 'a

        let handler = handler
        let headers = Http.Header.init ()
        let timeout = None

        let protocol : ('value, 'value t, 'value t, error) Client.Protocol.t =
          [ status ]
      end) in
      let client = C.v in
      let* () =
        let uri = uri server "/404" in
        let+ result = Client.get client (Schema payload_schema) uri in
        Alcotest.(check ~here:[%here] (result payload error))
          "request failed"
          (Result.Error
             {
               method_ = `GET;
               reason = `Unexpected_status `Not_found;
               status = Some `Not_found;
               uri;
             })
          result
      in
      let+ result = Client.get client Empty @@ uri server "/empty" in
      Alcotest.(check ~here:[%here] (result unit error))
        "request failed" (Result.Ok ()) result
  end

  module Timeout = struct
    let _timeout server handler timeout should_complete status chunk_received =
      let client = Client.make handler in
      Future.Scope.run ~name:"read stream" @@ fun scope ->
      let uri = uri server "/big-slow" in
      let* result = Client.get ~timeout client (Channel scope) uri in
      let () = Logs.info (fun m -> m "headers received") in
      let+ result =
        match result with
        | Result.Error _ as error -> Future.return error
        | Result.Ok stream ->
          Future.catch
            (fun () ->
              let* first_chunk = Future.In_channel.read_string stream in
              let () = Logs.info (fun m -> m "first body chunk received") in
              let () = chunk_received () in
              let+ content = Future.In_channel.read_all_string stream in
              Result.Ok (first_chunk ^ content))
            (function
              | Schematic_http.Channel error ->
                Result.Error
                  (error
                    : [ `Networking ] Schematic_http.Error.t
                    :> [> `Networking ] Schematic_http.Error.t)
                |> Future.return
              | exn -> raise exn)
      in
      let name, expected =
        if should_complete then ("completed", Result.Ok Data.v)
        else
          ( "timed out",
            Result.Error
              {
                Schematic_http.Error.method_ = `GET;
                reason = `Networking;
                status;
                uri;
              } )
      in
      Alcotest.(check ~here:[%here] (result string error))
        ("request " ^ name) expected result

    let headers server handler =
      _timeout server handler (Timmy.Span.seconds 1) false None (fun () ->
          Alcotest.fail ~here:[%here] "body chunk was received")

    let body server handler =
      let chunk_received = ref false in
      let+ () =
        _timeout server handler (Timmy.Span.seconds 3) false (Some `OK)
          (fun () -> chunk_received := true)
      in
      Alcotest.(check ~here:[%here] bool)
        "body was partially received" true !chunk_received

    let complete mono_clock server handler =
      let+ () =
        _timeout server handler (Timmy.Span.seconds 20) true (Some `OK)
          (fun () -> ())
      in
      sleep_and_advance ~flush:true mono_clock 20.
  end

  let run run test_case =
    Eio_main.run @@ fun env ->
    let mono_clock = Eio_mock.Clock.Mono.make ()
    and net = Eio.Stdenv.net env in
    let clock =
      (* stolen from Eio_mock.Backend *)
      let handler =
        Eio.Time.Pi.clock
          (module struct
            type t = Eio_mock.Clock.Mono.t
            type time = float

            let wall_of_mtime m = Int64.to_float (Mtime.to_uint64_ns m) /. 1e9
            let wall_to_mtime w = Mtime.of_uint64_ns (Int64.of_float (w *. 1e9))
            let now t = wall_of_mtime (Eio.Time.Mono.now t)

            let sleep_until t time =
              Eio.Time.Mono.sleep_until t (wall_to_mtime time)
          end)
      in
      Eio.Resource.T (mono_clock, handler)
    in
    Eio.Switch.run @@ fun sw ->
    let server =
      let port = server mono_clock net sw in
      { port }
    in
    let test_case name speed f =
      test_case name speed @@ fun handler ->
      let+ () = f handler in
      let (_ : bool) = Eio_mock.Clock.Mono.try_advance mono_clock in
      ()
    in
    run clock mono_clock net
      [
        ( "success",
          [
            test_case "GET empty" `Quick (Requests.get_empty server);
            test_case "GET json" `Quick (Requests.get_json server);
            test_case "GET string" `Quick (Requests.get_string server);
            test_case "GET stream" `Quick (Requests.get_stream server);
            test_case "GET slow stream" `Quick (Requests.get_stream_slow server);
            test_case "POST json" `Quick (Requests.post server);
          ] );
        ( "protocol",
          [
            test_case "404" `Quick (Protocol.not_found server);
            test_case "unexpected status" `Quick
              (Protocol.unexpected_status server);
          ] );
        ( "timeout",
          [
            test_case "headers" `Quick (Timeout.headers server);
            test_case "body" `Quick (Timeout.body server);
            test_case "complete" `Quick (Timeout.complete mono_clock server);
          ] );
      ]
end

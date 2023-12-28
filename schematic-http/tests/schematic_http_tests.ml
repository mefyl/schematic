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

type server = { port : int }

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
    let callback connection request body =
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
    let rec serve () =
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

let error =
  Alcotest.testable
    Schematic_http.Error.(
      pp @@ pp_reason @@ Fmt.of_to_string Cohttp.Code.string_of_status)
    Schematic_http.Error.(equal @@ equal_reason @@ Stdlib.( = ))

module Make
    (Future : Nofuture.S)
    (Handler : Schematic_http.HANDLER
                 with type 'a result = 'a
                  and type error = Schematic_http.Error.never
                  and module Future = Future) =
struct
  let ( let* ) v f = Future.bind v ~f
  let ( let+ ) v f = Future.map v ~f

  type server = { port : int }

  module Client =
    Schematic_http.Make
      (Schematic_http.Map_error
         (Schematic_http.Check_status (Schematic_http.Status_success) (Handler))
            (struct
              type t = Cohttp.Code.status_code

              let map = function
                | Schematic_http.Error.Unexpected_status status -> status
                | Valid_status (_ : Schematic_http.Error.never) -> .
            end))

  module Requests = struct
    let get_empty { port } client =
      let+ result =
        Client.get client Empty
        @@ Uri.make ~scheme:"http"
             ~host:Eio.Net.Ipaddr.(Fmt.str "%a" pp V4.loopback)
             ~port ~path:"/empty" ()
      in
      Alcotest.(check ~here:[%here] (result unit error))
        "request succeded" (Result.Ok ()) result

    let get_json { port } client =
      let+ result =
        Client.get client (Schema payload_schema)
        @@ Uri.make ~scheme:"http"
             ~host:Eio.Net.Ipaddr.(Fmt.str "%a" pp V4.loopback)
             ~port ~path:"/const" ()
      in
      Alcotest.(check ~here:[%here] (result payload error))
        "request succeded" (Result.Ok const_payload) result

    let get_string { port } client =
      let+ result =
        Client.get client String
        @@ Uri.make ~scheme:"http"
             ~host:Eio.Net.Ipaddr.(Fmt.str "%a" pp V4.loopback)
             ~port ~path:"/const" ()
      in
      Alcotest.(check ~here:[%here] (result string error))
        "request succeded"
        (Result.Ok (Schematic.Json.String.encode payload_schema const_payload))
        result

    let _get_stream port client path =
      Future.Scope.run ~name:"read stream" @@ fun scope ->
      let* result =
        Client.get client (Channel scope)
        @@ Uri.make ~scheme:"http"
             ~host:Eio.Net.Ipaddr.(Fmt.str "%a" pp V4.loopback)
             ~port ~path ()
      in
      let+ result =
        match result with
        | Result.Error _ as error -> Future.return error
        | Result.Ok stream ->
          let+ content = Future.In_channel.read_all_string stream in
          Result.Ok content
      in
      Alcotest.(check ~here:[%here] (result string error))
        "request succeded" (Result.Ok Data.v) result

    let get_stream { port } client = _get_stream port client "/big"
    let get_stream_slow { port } client = _get_stream port client "/big-slow"

    let post { port } client =
      let post_payload = { x = 107; y = "44" } in
      let+ result =
        Client.post client
          ~body:(Schema payload_schema, post_payload)
          (Schema payload_schema)
        @@ Uri.make ~scheme:"http"
             ~host:Eio.Net.Ipaddr.(Fmt.str "%a" pp V4.loopback)
             ~port ~path:"/cat" ()
      in
      Alcotest.(check ~here:[%here] (result payload error))
        "request succeded" (Result.Ok post_payload) result
  end

  module Timeout = struct
    let _timeout { port } client timeout should_complete chunk_received =
      Future.Scope.run ~name:"read stream" @@ fun scope ->
      let uri =
        Uri.make ~scheme:"http"
          ~host:Eio.Net.Ipaddr.(Fmt.str "%a" pp V4.loopback)
          ~port ~path:"/big-slow" ()
      in
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
                status = None;
                uri;
              } )
      in
      Alcotest.(check ~here:[%here] (result string error))
        ("request " ^ name) expected result

    let headers server client =
      _timeout server client (Timmy.Span.seconds 1) false (fun () ->
          Alcotest.fail ~here:[%here] "body chunk was received")

    let body server client =
      let chunk_received = ref false in
      let+ () =
        _timeout server client (Timmy.Span.seconds 3) false (fun () ->
            chunk_received := true)
      in
      Alcotest.(check ~here:[%here] bool)
        "body was partially received" true !chunk_received

    let complete mono_clock server client =
      let+ () =
        _timeout server client (Timmy.Span.seconds 20) true (fun () -> ())
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
      test_case name speed @@ fun client ->
      let+ () = f client in
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
        ( "timeout",
          [
            test_case "headers" `Quick (Timeout.headers server);
            test_case "body" `Quick (Timeout.body server);
            test_case "complete" `Quick (Timeout.complete mono_clock server);
          ] );
      ]
end

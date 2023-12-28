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

let server clock net sw =
  let server =
    let callback connection request body =
      match Cohttp.Request.uri request |> Uri.path with
      | "/big" ->
        let response = Cohttp.Response.make ()
        and body = Cohttp_eio.Body.of_string Data.v in
        (response, body)
      | "/big-slow" ->
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
              let () = Eio.Time.sleep clock 0.01 in
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
  let port =
    8123
    (* Really want to use 0 here, but eio doesn't support reading back the port
       from the socket *)
  in
  let socket =
    Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let () =
    Eio.Fiber.fork ~sw @@ fun () ->
    Cohttp_eio.Server.run
      ~on_error:(fun exn -> Alcotest.fail @@ Printexc.to_string exn)
      socket server
  in
  port

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
    let get_empty port client =
      let+ result =
        Client.get client Empty
        @@ Uri.make ~scheme:"http"
             ~host:Eio.Net.Ipaddr.(Fmt.str "%a" pp V4.loopback)
             ~port ~path:"/empty" ()
      in
      Alcotest.(check ~here:[%here] (result unit error))
        "request succeded" (Result.Ok ()) result

    let get_json port client =
      let+ result =
        Client.get client (Schema payload_schema)
        @@ Uri.make ~scheme:"http"
             ~host:Eio.Net.Ipaddr.(Fmt.str "%a" pp V4.loopback)
             ~port ~path:"/const" ()
      in
      Alcotest.(check ~here:[%here] (result payload error))
        "request succeded" (Result.Ok const_payload) result

    let get_string port client =
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
      Future.Scope.run @@ fun scope ->
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

    let get_stream port client = _get_stream port client "/big"
    let get_stream_slow port client = _get_stream port client "/big-slow"

    let post port client =
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

  let run run test_case =
    Eio_main.run @@ fun env ->
    let clock = Eio_mock.Clock.make ()
    and mono_clock = Eio_mock.Clock.Mono.make ()
    and net = Eio.Stdenv.net env in
    Eio.Switch.run @@ fun sw ->
    let () =
      Eio.Std.Fiber.fork_daemon ~sw @@ fun () ->
      let rec advance () =
        let () =
          if Eio_mock.Clock.Mono.try_advance mono_clock then
            Logs.debug (fun m -> m "advanced monotonic time");
          if Eio_mock.Clock.try_advance clock then
            Logs.debug (fun m -> m "advanced wall clock time")
        in
        let () = Eio.Fiber.yield () in
        advance ()
      in
      advance ()
    in
    let port = server clock net sw in
    run clock mono_clock net
      [
        ( "success",
          [
            test_case "GET empty" `Quick (Requests.get_empty port);
            test_case "GET json" `Quick (Requests.get_json port);
            test_case "GET string" `Quick (Requests.get_string port);
            test_case "GET stream" `Quick (Requests.get_stream port);
            test_case "GET stream" `Quick (Requests.get_stream_slow port);
            test_case "POST json" `Quick (Requests.post port);
          ] );
      ]
end

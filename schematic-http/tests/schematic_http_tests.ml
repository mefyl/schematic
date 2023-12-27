module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

type payload = {
  x : int;
  y : string;
}
[@@deriving eq, schema, show]

let const_payload = { x = 26; y = "105" }
let payload = Alcotest.testable pp_payload equal_payload

let server env sw =
  let server =
    let callback connection request body =
      match Cohttp.Request.uri request |> Uri.path with
      | "/big" ->
        let response = Cohttp.Response.make ()
        and body = Cohttp_eio.Body.of_string Data.v in
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
    Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
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
        Client.get client Client.empty
        @@ Uri.make ~scheme:"http"
             ~host:Eio.Net.Ipaddr.(Fmt.str "%a" pp V4.loopback)
             ~port ~path:"/empty" ()
      in
      Alcotest.(check ~here:[%here] (result unit error))
        "request succeded" (Result.Ok ()) result

    let get_json port client =
      let+ result =
        Client.get client payload_schema
        @@ Uri.make ~scheme:"http"
             ~host:Eio.Net.Ipaddr.(Fmt.str "%a" pp V4.loopback)
             ~port ~path:"/const" ()
      in
      Alcotest.(check ~here:[%here] (result payload error))
        "request succeded" (Result.Ok const_payload) result

    let get_string port client =
      let+ result =
        Client.get client Client.string
        @@ Uri.make ~scheme:"http"
             ~host:Eio.Net.Ipaddr.(Fmt.str "%a" pp V4.loopback)
             ~port ~path:"/const" ()
      in
      Alcotest.(check ~here:[%here] (result string error))
        "request succeded"
        (Result.Ok (Schematic.Json.String.encode payload_schema const_payload))
        result

    let get_stream port client =
      let* result =
        Client.get client Client.stream
        @@ Uri.make ~scheme:"http"
             ~host:Eio.Net.Ipaddr.(Fmt.str "%a" pp V4.loopback)
             ~port ~path:"/big" ()
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

    let post port client =
      let post_payload = { x = 107; y = "44" } in
      let+ result =
        Client.post client ~body:(payload_schema, post_payload) payload_schema
        @@ Uri.make ~scheme:"http"
             ~host:Eio.Net.Ipaddr.(Fmt.str "%a" pp V4.loopback)
             ~port ~path:"/cat" ()
      in
      Alcotest.(check ~here:[%here] (result payload error))
        "request succeded" (Result.Ok post_payload) result
  end

  let run run test_case =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let port = server env sw in
    run env
      [
        ( "success",
          [
            test_case "GET empty" `Quick (Requests.get_empty port);
            test_case "GET json" `Quick (Requests.get_json port);
            test_case "GET string" `Quick (Requests.get_string port);
            test_case "GET stream" `Quick (Requests.get_stream port);
            test_case "POST json" `Quick (Requests.post port);
          ] );
      ]
end

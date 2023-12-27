let ( let* ) = Lwt.( >>= )
let ( let+ ) = Lwt.( >|= )

include
  Schematic_http_tests.Make
    (Nofuture_lwt)
    (Schematic_cohttp_lwt.Make (Cohttp_lwt_unix.Client) (Schematic.Json.String))

let () = Logs.set_reporter @@ Logs_fmt.reporter ()
let () = Logs.Src.set_level Schematic_http.src @@ Some Debug

let () =
  let test_suite env tests =
    Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
    Lwt_eio.run_lwt @@ fun () ->
    let client = Schematic_cohttp_lwt.make () in
    Alcotest_lwt.(
      run_with_args "schematic-cohttp-lwt" (Cmdliner.Term.const client) tests)
  and test_case name mode f =
    Alcotest_lwt.test_case name mode (fun _ client -> f client)
  in
  run test_suite test_case

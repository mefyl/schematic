include
  Schematic_http_tests.Make
    (Nofuture_lwt_unix)
    (Schematic_cohttp_lwt.Make (Nofuture_lwt_unix) (Cohttp_lwt_unix.Client)
       (Schematic.Json.String))

let () =
  Eio_main.run @@ fun env ->
  let mono_clock = Eio_mock.Clock.Mono.make () in
  let test_suite clock _ _ tests =
    Lwt_eio.with_event_loop ~clock @@ fun _ ->
    Lwt_eio.run_lwt @@ fun () ->
    let client = Schematic_cohttp_lwt.make () in
    Alcotest_lwt.(
      run_with_args "schematic-cohttp-lwt" (Cmdliner.Term.const client) tests)
  and test_case name mode f =
    Alcotest_lwt.test_case name mode (fun _ client -> f client)
  in
  run env mono_clock test_suite test_case

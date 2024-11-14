let () = Logs.set_reporter @@ Logs_fmt.reporter ()
let () = Logs.Src.set_level Schematic_http.src @@ Some Debug

let () =
  Eio_main.run @@ fun env ->
  let mono_clock = Eio_mock.Clock.Mono.make () in
  let module Nofuture_eio = struct
    include Nofuture_eio

    let sleep s = Eio.Time.Mono.sleep mono_clock s
  end in
  let module Tests =
    Schematic_http_tests.Make
      (Nofuture_eio)
      (Schematic_cohttp_eio.Make (Nofuture_eio) (Schematic.Json.String))
  in
  let test_suite clock mono_clock net tests =
    let client = Schematic_cohttp_eio.make ~clock ~mono_clock ~net () in
    Alcotest.(
      run_with_args "schematic-cohttp-eio" (Cmdliner.Term.const client) tests)
  and test_case = Alcotest.test_case in
  Tests.run env mono_clock test_suite test_case

let () = Logs.set_reporter @@ Logs_fmt.reporter ()
let () = Logs.Src.set_level Schematic_http.src @@ Some Debug

module Tests =
  Schematic_http_tests.Make
    (Nofuture_eio)
    (Schematic_cohttp_eio.Make (Schematic.Json.String))

let () =
  let test_suite env tests =
    let client =
      Schematic_cohttp_eio.make ~clock:(Eio.Stdenv.clock env)
        ~mono_clock:(Eio.Stdenv.mono_clock env)
        ~net:(Eio.Stdenv.net env)
    in
    Alcotest.(
      run_with_args "schematic-cohttp-eio" (Cmdliner.Term.const client) tests)
  and test_case = Alcotest.test_case in
  Tests.run test_suite test_case

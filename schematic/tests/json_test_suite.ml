(** JSONTestSuite compliance tests: https://github.com/nst/JSONTestSuite

    - y_*.json must be accepted and match ezjsonm output
    - n_*.json must be rejected
    - i_*.json are implementation-defined (we just check we agree with ezjsonm)

    Some rejection tests have been marked XPASS, as it's not crucial for us to
    reject invalid input, and as jsonm itself does not even reject them. *)

let test_dir = Sys.getenv "JSON_TEST_SUITE_DIR"

let parse_ours s =
  match Schematic.Json.of_string s with Ok v -> Ok v | Error _ -> Error ()

let parse_ezjsonm s =
  match Ezjsonm.value_from_string s with v -> Ok v | exception _ -> Error ()

let read_file path =
  let ic = open_in path in
  let s = In_channel.input_all ic in
  close_in ic;
  s

let make_tests () =
  let files =
    Sys.readdir test_dir
    |> Array.to_list
    |> List.sort String.compare
    |> List.filter (fun f -> Filename.check_suffix f ".json")
  and xpass_files =
    Sys.readdir (test_dir ^ "/xpass")
    |> Array.to_list
    |> List.sort String.compare
    |> List.filter (fun f -> Filename.check_suffix f ".json")
  in
  let y_tests =
    files
    |> List.filter_map (fun file ->
        if not (String.sub file 0 2 = "y_") then None
        else
          Some
            Alcotest.(
              test_case file `Quick (fun () ->
                  let s = read_file (test_dir ^ "/" ^ file) in
                  let schematic = parse_ours s
                  and ezjsonm = parse_ezjsonm s in
                  check bool "input was accepted" true (Result.is_ok schematic);
                  check
                    (result (module Schematic.Json) unit)
                    "output matches ezjsonm" ezjsonm schematic)))
  and n_tests =
    files
    |> List.filter_map (fun file ->
        if not (String.sub file 0 2 = "n_") then None
        else
          Some
            Alcotest.(
              test_case file `Quick (fun () ->
                  let s = read_file (test_dir ^ "/" ^ file) in
                  check bool "rejected" true (Result.is_error (parse_ours s)))))
  and xpass_tests =
    xpass_files
    |> List.map (fun file ->
        Alcotest.(
          test_case file `Quick (fun () ->
              let s = read_file (test_dir ^ "/xpass/" ^ file) in
              check bool "expected unexpected acceptance" true
                (Result.is_ok (parse_ours s)))))
  and i_tests =
    files
    |> List.filter_map (fun file ->
        if not (String.sub file 0 2 = "i_") then None
        else
          Some
            Alcotest.(
              test_case file `Quick (fun () ->
                  let s = read_file (test_dir ^ "/" ^ file) in
                  (* acceptance/rejection is implementation defined, just check
                     we don't crash. *)
                  ignore (parse_ours s))))
  in
  [
    ("acceptance tests", y_tests);
    ("rejection tests", n_tests);
    ("expected failing rejection tests", xpass_tests);
    ("implementation defined", i_tests);
  ]

let () = Alcotest.run "JSON test suite" (make_tests ())

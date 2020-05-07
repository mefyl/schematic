open Batteries

module Parse_error = struct
  let empty () = Schematic.Json.of_string ""

  (* let trailing_garbage () = Schematic.Json.of_string "{ \"a\" : 42 }{" *)

  let missing_quotes () = Schematic.Json.of_string "{ a : 42 }"
end

let () =
  let open Alcotest in
  let error_case name f =
    test_case name `Quick (fun () ->
        check bool "error" true (Result.is_error @@ f ()))
  in
  run "Schematic.Json"
    [ ( "parse_error"
      , [ error_case "empty" Parse_error.empty
          (* FIXME *)
          (* ; error_case "trailing garbage" Parse_error.trailing_garbage *)
        ; error_case "missing quotes" Parse_error.missing_quotes
        ] )
    ]

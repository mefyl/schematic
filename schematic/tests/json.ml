open Base

module Parse_error = struct
  let empty () = Schematic.Json.of_string ""

  (* let trailing_garbage () = Schematic.Json.of_string "{ \"a\" : 42 }{" *)
  let missing_quotes () = Schematic.Json.of_string "{ a : 42 }"
end

module Pretty_print = struct
  let check ~here name json repr =
    let buffer = Buffer.create 1024 in
    let fmt = Stdlib.Format.formatter_of_buffer buffer in
    let () = Stdlib.Format.pp_set_margin fmt 20 in
    let () = Fmt.pf fmt "%a" Schematic.Json.pp json in
    let () = Stdlib.Format.pp_print_flush fmt () in
    Alcotest.(check ~here string name repr (Buffer.contents buffer))

  let null () = check ~here:[%here] "null" `Null "null"

  let bool () =
    check ~here:[%here] "true" (`Bool true) "true";
    check ~here:[%here] "false" (`Bool false) "false"

  let float () = check ~here:[%here] "float" (`Float 123.45) "123.45"
  let string () = check ~here:[%here] "string" (`String "foobar") {|"foobar"|}

  let array () =
    check ~here:[%here] "empty" (`A []) "[]";
    check ~here:[%here] "fits"
      (`A [ `Float 0.; `String "1"; `Bool false ])
      {|[0, "1", false]|};
    check ~here:[%here] "overflow"
      (`A [ `Float 0.123; `String "1.456"; `Bool false ])
      {|[
   0.123,
   "1.456",
   false
]|}

  let object_ () =
    check ~here:[%here] "empty" (`O []) "{}";
    check ~here:[%here] "fits"
      (`O [ ("a", `Float 0.); ("b", `String "1") ])
      {|{"a": 0, "b": "1"}|};
    check ~here:[%here] "overflow"
      (`O [ ("a", `Float 0.123); ("b", `String "1.456"); ("c", `Bool false) ])
      {|{
   "a": 0.123,
   "b": "1.456",
   "c": false
}|};
    check ~here:[%here] "overflow-item"
      (`O
         [
           ("a", `Float 0.123); ("b", `String "1.123456789"); ("c", `Bool false);
         ])
      {|{
   "a": 0.123,
   "b":
     "1.123456789",
   "c": false
}|}
end

let () =
  let open Alcotest in
  let error_case ~here name f =
    test_case name `Quick (fun () ->
        check ~here bool "error" true (Result.is_error @@ f ()))
  and test_case name f = test_case name `Quick f in
  run "Schematic.Json"
    [
      ( "parse_error",
        [
          error_case ~here:[%here] "empty" Parse_error.empty;
          (* error_case ~here:[%here] "trailing garbage"
           *   Parse_error.trailing_garbage; *)
          error_case ~here:[%here] "missing quotes" Parse_error.missing_quotes;
        ] );
      ( "pretty-print",
        [
          test_case "null" Pretty_print.null;
          test_case "bool" Pretty_print.bool;
          test_case "float" Pretty_print.float;
          test_case "string" Pretty_print.string;
          test_case "array" Pretty_print.array;
          test_case "object" Pretty_print.object_;
        ] );
    ]

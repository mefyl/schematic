open Base

let json_testable = Alcotest.testable Schematic.Json.pp Schematic.Json.equal

let schematic_error_decoding_testable =
  Alcotest.testable Schematic.Error.pp_decoding Schematic.Error.equal_decoding

type ptime_date = int * int * int [@@deriving show]

let ptime_date_testable =
  Alcotest.testable pp_ptime_date Schematic.Schemas.Ptime.equal_date

(** [check ~here value_testable ?kotlin_datatype schema value]:
    - transforms [value] into a Kotlin-compatible JSON using [schema]
    - sends this JSON to the test Kotlin application (passing as data type the
      [kotlin_datatype] if present, otherwise guessed from [schema]), which
      decodes it into Kotlin, encodes it into JSON, and then sends it back
    - transforms the received Kotlin-compatible JSON into OCaml using [schema]
    - checks that the OCaml values are equal using [value_testable] *)
let check ~here value_testable ?kotlin_datatype (schema : _ Schematic.Schema.t)
    value =
  let kotlin_datatype =
    Option.map schema.id ~f:(fun schema_id ->
        "co.routine." ^ Schematic.Kotlin.to_type_name schema_id)
    |> Option.first_some kotlin_datatype
    |> Option.value_exn ~here
  in
  let json_sent = Schematic.Kotlin.to_json schema value in
  let command =
    Fmt.str "kotlin-app/bin/app '%s' '%s'" kotlin_datatype
      (Ezjsonm.value_to_string json_sent)
  in
  let in_channel = Unix.open_process_in command in
  let received_lines = In_channel.input_lines in_channel in
  let () = In_channel.close in_channel in
  let json_received = Ezjsonm.value_from_string (List.hd_exn received_lines) in
  let actual = Schematic.Kotlin.from_json schema json_received in
  Alcotest.(
    check ~here
      (result value_testable schematic_error_decoding_testable)
      "JSON values do not match after a roundtrip" (Ok value) actual)

let test_bool () : unit =
  let schema = Schematic.Schemas.bool_schema in
  check ~here:[%here] Alcotest.bool ~kotlin_datatype:"java.lang.Boolean" schema
    true

let test_date () : unit =
  let schema = Schematic.Schemas.Ptime.date_schema in
  check ~here:[%here] ptime_date_testable
    ~kotlin_datatype:"kotlinx.datetime.LocalDate" schema (2024, 9, 26)

let test_int () : unit =
  let schema = Schematic.Schemas.int_schema in
  check ~here:[%here] Alcotest.int ~kotlin_datatype:"java.lang.Integer" schema
    42

let test_json () : unit =
  let schema = Schematic.Schemas.json_schema in
  check ~here:[%here] json_testable
    ~kotlin_datatype:"kotlinx.serialization.json.JsonElement" schema `Null;
  check ~here:[%here] json_testable
    ~kotlin_datatype:"kotlinx.serialization.json.JsonElement" schema
    (`Bool false);
  check ~here:[%here] json_testable
    ~kotlin_datatype:"kotlinx.serialization.json.JsonElement" schema
    (`Float 42.18);
  check ~here:[%here] json_testable
    ~kotlin_datatype:"kotlinx.serialization.json.JsonElement" schema
    (`String "hello world");
  check ~here:[%here] json_testable
    ~kotlin_datatype:"kotlinx.serialization.json.JsonElement" schema
    (`O [ ("a", `Bool true); ("b", `Null) ]);
  check ~here:[%here] json_testable
    ~kotlin_datatype:"kotlinx.serialization.json.JsonElement" schema
    (`A [ `Null; `Bool true; `String "foobar" ])

let test_string () : unit =
  let schema = Schematic.Schemas.string_schema in
  check ~here:[%here] Alcotest.string ~kotlin_datatype:"java.lang.String" schema
    "hello world"

let test_foo () : unit =
  let schema = Test_utils.Tests.foo_schema in
  let value_testable =
    Alcotest.testable Test_utils.Tests.pp_foo Test_utils.Tests.equal_foo
  in
  check ~here:[%here] value_testable schema (A 42);
  check ~here:[%here] value_testable schema
    (B { bar = "hello world"; baz = true })

let () =
  Alcotest.(
    run "Roundtrip OCaml -> JSON -> Kotlin -> JSON -> OCaml"
      [
        ( "Basic types",
          [
            test_case "bool" `Quick test_bool;
            test_case "date" `Quick test_date;
            test_case "int" `Quick test_int;
            test_case "json" `Quick test_json;
            test_case "string" `Quick test_string;
          ] );
        ("Complex types", [ test_case "foo" `Quick test_foo ]);
      ])

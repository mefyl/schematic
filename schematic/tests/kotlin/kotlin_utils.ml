open Base

let () = Logs.set_reporter (Logs_fmt.reporter ())
let () = Logs.set_level (Some Logs.Info)

let schematic_error_decoding_testable =
  Alcotest.testable Schematic.Error.pp_decoding Schematic.Error.equal_decoding

module Tests = Test_utils.Tests.Make (struct
  type t = Ezjsonm.value

  let name = "Kotlin"
  let encode = Schematic.Kotlin.to_json
  let decode = Schematic.Kotlin.from_json
  let t = Alcotest.testable Schematic.Json.pp Schematic.Json.equal

  (** The JSONs in [Tests.Make] are incompatible with the JSONs generated with
      the Kotlin flavor. *)
  let of_json _json = None

  let local_time_support = true

  (** In a later MR we will call the roundtrip test here *)
  let extra_test
      (Test_utils.Tests.Test
        {
          here;
          schema;
          kotlin_class_name;
          is_kotlin_roundtrip_enabled;
          value;
          value_testable;
          _;
        }) _encoded =
    if is_kotlin_roundtrip_enabled then
      let kotlin_datatype =
        Option.map schema.id ~f:(fun schema_id ->
            "co.routine." ^ Schematic.Kotlin.to_type_name schema_id)
        |> Option.first_some kotlin_class_name
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
      let () =
        Fmt.epr "Received lines: %a@." (Fmt.list Fmt.string) received_lines
      in
      let json_received =
        Ezjsonm.value_from_string (List.hd_exn received_lines)
      in
      let actual = Schematic.Kotlin.from_json schema json_received in
      Alcotest.(
        check ~here
          (result value_testable schematic_error_decoding_testable)
          "JSON values do not match after a roundtrip" (Ok value) actual)
end)

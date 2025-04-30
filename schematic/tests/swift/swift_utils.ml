open Base

let () = Logs.set_reporter (Logs_fmt.reporter ())
let () = Logs.set_level (Some Logs.Info)

let schematic_error_decoding_testable =
  Alcotest.testable Schematic.Error.pp_decoding Schematic.Error.equal_decoding

module Tests = Test_utils.Tests.Make (struct
  type t = Ezjsonm.value

  let name = "Swift"
  let encode = Schematic.Swift.to_json
  let decode = Schematic.Swift.from_json
  let t = Alcotest.testable Schematic.Json.pp Schematic.Json.equal

  (** The JSONs in [Tests.Make] are incompatible with the JSONs generated with
      the Swift flavor. *)
  let of_json _json = None

  let local_time_support = true

  (** In a later MR we will call the roundtrip test here *)
  let extra_test
      (Test_utils.Tests.Test
         {
           here;
           schema;
           swift_class_name;
           is_swift_roundtrip_enabled;
           value;
           value_testable;
           _;
         }) _encoded =
    if is_swift_roundtrip_enabled then
      let swift_datatype =
        schema.id
        |> Option.first_some swift_class_name
        |> Option.map ~f:(fun schema_id ->
               Schematic.Swift.to_type_name schema_id)
        |> Option.value_exn ~here
      in
      let json_sent = Schematic.Swift.to_json schema value in
      let command =
        Fmt.str "./swift-app '%s' '%s'" swift_datatype
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
      let actual = Schematic.Swift.from_json schema json_received in
      Alcotest.(
        check ~here
          (result value_testable schematic_error_decoding_testable)
          "JSON values do not match after a roundtrip" (Ok value) actual)
end)

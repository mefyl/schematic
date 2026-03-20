open Base

module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

let dollar = Base.String.Search_pattern.create "$"
let root = Stdlib.Filename.dirname @@ Stdlib.Sys.executable_name

module Tests = Test_utils.Tests.Make (struct
  include Schematic.Json

  let t = Alcotest.testable Schematic.Json.pp Schematic.Json.equal
  let of_json json = Some json
  let local_time_support = true
  let streaming = Some (module Schematic.Json.Streaming : Schematic.Streaming.S)

  let extra_test (Test_utils.Tests.Test { here; schema; _ }) encoded =
    let schema =
      Schematic.Json.Schema.of_schema schema
      |> Schematic.Json.encode Schematic.Json.Schema.schema
    in
    Fmt.epr "%s@." Ezjsonm.(value_to_string schema);
    let cmd =
      Printf.sprintf {|%s/validate-schema "%s" "%s"|} root
        (Base.String.Search_pattern.replace_all dollar
           ~in_:(Base.String.escaped @@ Schematic.Json.to_string @@ schema)
           ~with_:"\\$")
        (Base.String.escaped @@ Schematic.Json.to_string @@ encoded)
    in
    if not @@ Int.equal (Stdlib.Sys.command cmd) 0 then
      Alcotest.check ~here t
        (Fmt.str "%s: schema validation" name)
        schema encoded
end)

let streaming_lazy_sequence () =
  let open struct
    type batch = {
      etag : int;
      commits :
        (commit Base.Sequence.t
        [@schematic.schema
          fun _ ->
            Schematic.Schema.make
            @@ Sequence
                 {
                   construct = Result.return;
                   deconstruct = Fn.id;
                   item = [%schema: int];
                   length = Base.Sequence.length;
                 }]);
    }

    and commit = int [@@deriving schema]
  end in
  let buffer = Buffer.create 4096
  and current_length = ref 0 in
  Schematic.Json.Streaming.to_buffer batch_schema
    {
      etag = 0;
      commits =
        Base.Sequence.(
          of_list [ 0; 1; 2; 3; 4 ]
          |> map ~f:(fun i ->
              let length = Buffer.length buffer in
              let () =
                Alcotest.(check bool)
                  "buffer is filled as we lazily process the sequence" true
                  (length > !current_length)
              in
              let () = current_length := length in
              i));
    }
    buffer

let () =
  Alcotest.run "Schematic.Schema"
  @@ Tests.tests
  @ [
      ( "Streaming",
        [ Alcotest.test_case "lazy sequence" `Quick streaming_lazy_sequence ] );
    ]

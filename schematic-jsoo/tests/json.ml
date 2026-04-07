let () =
  let module Json = Test_utils.Tests.Make (struct
    include Schematic_jsoo.Json

    let local_time_support = true
    let t = Alcotest.testable Schematic_jsoo.Json.pp Schematic_jsoo.Json.equal
    let of_json json = Some json
    let extra_test _ _ = ()

    let streaming =
      Some (module Schematic.Json.Streaming : Schematic.Streaming.S)
  end) in
  Alcotest.run "Schematic_jsoo.Json" Json.tests

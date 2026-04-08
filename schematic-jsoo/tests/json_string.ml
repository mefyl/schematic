let () =
  let module Json = Test_utils.Tests.Make (struct
    include Schematic_jsoo.Json.String

    let local_time_support = true
    let t = Alcotest.string
    let of_json _ = None
    let extra_test _ _ = ()
    let streaming = None
  end) in
  Alcotest.run "Schematic_jsoo.Json.String" Json.tests

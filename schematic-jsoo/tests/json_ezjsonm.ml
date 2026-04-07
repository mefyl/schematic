(** Unlike [json_string.ml], this test truly roundtrips through the ezjsonm
    representation, while the latter is optimized to work directly on the jsoo
    representation. *)

let () =
  let module Json = Test_utils.Tests.Make (struct
    include Schematic.Coding.Make_string (Schematic_jsoo.Json)

    let local_time_support = true
    let t = Alcotest.string
    let of_json _ = None
    let extra_test _ _ = ()

    let streaming =
      Some (module Schematic.Json.Streaming : Schematic.Streaming.S)
  end) in
  Alcotest.run "Schematic.Coding.Make_string (Schematic_jsoo.Json) " Json.tests

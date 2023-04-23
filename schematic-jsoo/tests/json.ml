module Json = Test_utils.Tests.Make (struct
  include Schematic_jsoo.Json.String

  let to_string s = s
  let local_time_support = true

  let t =
    Alcotest.testable
      (fun fmt js -> Fmt.string fmt @@ to_string js)
      (fun l r -> String.equal (to_string l) (to_string r))

  let of_json _ = None
  let extra_test ~here:_ _ _ = ()
end)

let () = Alcotest.run "Schematic_jsoo.Json" Json.tests

module Tests = Test_utils.Tests.Make (struct
  include Schematic_jsoo.Jsoo

  let to_string js = Js_of_ocaml.(Json.output js |> Js.to_string)
  let local_time_support = false

  let t =
    Alcotest.testable
      (fun fmt js -> Fmt.string fmt @@ to_string js)
      (fun l r -> String.equal (to_string l) (to_string r))

  let of_json _ = None
  let extra_test ~here:_ _ _ = ()
end)

let () = Alcotest.run "Schematic.Schema" Tests.tests

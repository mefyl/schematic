open Base

module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

module Jsoo = Test_utils.Tests.Make (struct
  include Schematic_jsoo.Jsoo

  let to_string js = Js_of_ocaml.(Json.output js |> Js.to_string)
  let local_time_support = false

  let t =
    Alcotest.testable
      (fun fmt js -> Fmt.string fmt @@ to_string js)
      (fun l r -> String.equal (to_string l) (to_string r))

  let of_json _ = None
  let extra_test _ _ = ()
end)

module Errors = struct
  type either_opt = { v : (int, bool) Either.t option } [@@deriving schema]

  let undefined () =
    let res =
      Schematic_jsoo.Jsoo.decode either_opt_schema
        (Js_of_ocaml.Js.Unsafe.inject
           (object%js
              val v = Js_of_ocaml.Js.undefined
           end))
    in
    Alcotest.(
      check ~here:[%here] (result pass reject)
        "undefined in JSON does not choke")
      (Result.Ok { v = None })
      res

  type recurse =
    | Object of (string * recurse) list
    | String of string
  [@@deriving schema]

  (* This used to recurse infinitely because strings where considered valid
     objects because [Object.keys('test')] returns [[ '0', '1', '2', '3' ]] *)
  let object_keys () =
    let res =
      Schematic_jsoo.Jsoo.decode recurse_schema
        (Js_of_ocaml.Js.Unsafe.inject (Js_of_ocaml.Js.string "test"))
    in
    Alcotest.(
      check ~here:[%here] (result pass reject) "string is not seen as object")
      (Ok (String "test")) res

  let tests =
    Alcotest.
      [
        test_case "undefined" `Quick undefined;
        test_case "object_keys" `Quick object_keys;
      ]
end

let () =
  Alcotest.run "Schematic_jsoo.Jsoo" (("jsoo", Errors.tests) :: Jsoo.tests)

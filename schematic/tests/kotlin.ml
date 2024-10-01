module Tests = Test_utils.Tests.Make (struct
  type t = Ezjsonm.value

  let name = "Kotlin"
  let encode = Schematic.Kotlin.to_json
  let decode = Schematic.Kotlin.from_json
  let t = Alcotest.testable Schematic.Json.pp Schematic.Json.equal

  (** The JSONs in [Test_utils.Tests.Make] are incompatible with the JSONs
      generated with the Kotlin flavor. *)
  let of_json _json = None

  let local_time_support = true

  (** In a later MR we will call the roundtrip test here *)
  let extra_test ~here:_ _schema _encoded = ()
end)

let () = Alcotest.run "Schematic.Kotlin" Tests.tests

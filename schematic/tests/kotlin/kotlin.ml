open Base

let () =
  Alcotest.run "Schematic.Kotlin"
    (Kotlin_utils.Tests.tests @ Kotlin_utils.Tests.tests2)

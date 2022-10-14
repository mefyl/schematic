open Base
open Schematic_c
open Test_utils

module Generate_types = struct
  let test schema expected =
    let expected = Base.Set.of_list (module String) expected in
    let actual =
      C.of_schema schema
      |> List.filter_map ~f:(fun x ->
             C.Swift.type_decl x |> Option.map ~f:(fun x -> "\n" ^ x ^ "\n"))
      |> Base.Set.of_list (module String)
    in

    Alcotest.(
      check Testables.Set.String.set "Swift type declaration" expected actual)
end

module type Schema_test = sig
  val schema : Schematic.Schema.pack
  val swift : string
end

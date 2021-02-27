type record = {
  a : int;
  b : float option;
  c : string;
}

and nullable = bool option

and tuple = string * int64 * unit [@@deriving schema]

module Generate_types = struct
  let test t expected =
    Alcotest.(
      check string "typescript declarations" expected
      @@ Schematic.Typescript.(to_string @@ of_schema t))

  let nullable () = test nullable_schema "boolean | null"

  let record () =
    test record_schema {|{ "a": number; "b"?: number; "c": string }|}

  let tuple () = test tuple_schema "[ string, number, null ]"
end

let () =
  let open Alcotest in
  run "Schematic.Typescript"
    [
      ( "generate types",
        [
          test_case "nullable" `Quick Generate_types.nullable;
          test_case "record" `Quick Generate_types.record;
          test_case "tuple" `Quick Generate_types.tuple;
        ] );
    ]

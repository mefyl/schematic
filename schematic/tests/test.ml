let dollar = Base.String.Search_pattern.create "$"
let root = Stdlib.Filename.dirname @@ Stdlib.Sys.executable_name

module Tests = Test_utils.Tests.Make (struct
  include Schematic.Json

  let t = Alcotest.testable Schematic.Json.pp Schematic.Json.equal
  let of_json json = Some json
  let local_time_support = true

  let extra_test ~here schema encoded =
    let schema =
      Schematic.Json.Schema.of_schema schema
      |> Schematic.Json.encode Schematic.Json.Schema.schema
    in
    Fmt.epr "%s@." Ezjsonm.(value_to_string schema);
    let cmd =
      Printf.sprintf {|%s/validate-schema "%s" "%s"|} root
        (Base.String.Search_pattern.replace_all dollar
           ~in_:(Base.String.escaped @@ Schematic.Json.to_string @@ schema)
           ~with_:"\\$")
        (Base.String.escaped @@ Schematic.Json.to_string @@ encoded)
    in
    if Stdlib.Sys.command cmd <> 0 then
      Alcotest.check ~here t
        (Fmt.str "%s: schema validation" name)
        schema encoded
end)

let () = Alcotest.run "Schematic.Schema" Tests.tests

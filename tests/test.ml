open Base
open Poly

let root = Caml.Filename.dirname @@ Caml.Sys.executable_name

let json_schema schema =
  Schematic.Json.Schema.of_schema schema
  |> Schematic.Json.encode Schematic.Json.Schema.schema

let json = Alcotest.testable Schematic.Json.pp ( = )

let test name test schema value j =
  let encoded = Schematic.Json.encode schema value in
  let j =
    match Schematic.Json.of_string j with
    | Result.Ok parsed -> parsed
    | Result.Error _ -> Alcotest.fail "invalid JSON expectation"
  in
  Alcotest.check json name j encoded;
  let () =
    match Schematic.Json.decode schema j with
    | Result.Ok decoded -> Alcotest.check test name value decoded
    | Result.Error (`Json_decode e) ->
      Alcotest.fail (Printf.sprintf "JSON decode error: %s" e)
  in
  let schema = json_schema schema in
  let cmd =
    Printf.sprintf "%s/validate-schema \"%s\" \"%s\"" root
      (String.escaped @@ Schematic.Json.to_string @@ schema)
      (String.escaped @@ Schematic.Json.to_string @@ encoded)
  in
  if Caml.Sys.command cmd <> 0 then Alcotest.check json name schema encoded

(* Failure *)

let failure () =
  let open Schematic.Schema in
  try
    test "int atom" Alcotest.int int_schema 28 "29";
    Alcotest.fail "failure test failed"
  with Failure _ -> ()

(* Atoms *)

let atom_int () =
  let open Schematic.Schema in
  test "int atom" Alcotest.int int_schema 28 "28"

let atom_string () =
  let open Schematic.Schema in
  test "int atom" Alcotest.string string_schema "trente trois"
    "\"trente trois\""

(* Record *)

type version = {
  major : int;
  minor : int;
  subminor : int;
}
[@@deriving schema]

(* FIXME: Base complains that Caml.format is deprecated, but does not provide
   Base.Printf.fprintf *)
let fprintf = Caml.Format.fprintf

let pp_version fmt { major; minor; subminor } =
  fprintf fmt "%i.%i.%i" major minor subminor

let test_version = Alcotest.testable pp_version ( = )

let v1 = { major = 0; minor = 1; subminor = 2 }

and v2 = { major = 3; minor = 4; subminor = 5 }

and v3 = { major = 6; minor = 7; subminor = 8 }

let record () =
  test "record" test_version version_schema v1
    "{\"major\": 0, \"minor\": 1, \"subminor\": 2}"

(* Alias *)

type version_alias = version [@@deriving schema]

let alias () =
  test "record" test_version version_alias_schema v2
    "{\"major\": 3, \"minor\": 4, \"subminor\": 5}"

(* Tuple *)

type range = version * version * int [@@deriving schema]

let pp_range fmt (l, r, i) =
  fprintf fmt "(%a, %a, %i)" pp_version l pp_version r i

let test_range = Alcotest.testable pp_range ( = )

let tuple () =
  test "tuple" test_range range_schema (v1, v2, 83)
    "[{\"major\": 0, \"minor\": 1, \"subminor\": 2}, {\"major\": 3, \"minor\": \
     4, \"subminor\": 5}, 83]"

(* Variant *)

type expectation =
  | Exact of version
  | Range of range
  | Nothing
[@@deriving schema]

let pp_expectation fmt = function
  | Exact v -> pp_version fmt v
  | Range (l, r, i) -> fprintf fmt "(%a, %a, %i)" pp_version l pp_version r i
  | Nothing -> fprintf fmt "nothing"

let test_expectation = Alcotest.testable pp_expectation ( = )

let variant () =
  test "variant" test_expectation expectation_schema
    (Exact { major = 1; minor = 2; subminor = 3 })
    "{\"major\": 1, \"minor\": 2, \"subminor\": 3}";
  test "variant" test_expectation expectation_schema
    (Range
       ( { major = 1; minor = 2; subminor = 3 },
         { major = 4; minor = 5; subminor = 6 },
         108 ))
    "[{\"major\": 1, \"minor\": 2, \"subminor\": 3}, {\"major\": 4, \"minor\": \
     5, \"subminor\": 6}, 108]";
  test "variant" test_expectation expectation_schema Nothing "\"nothing\""

(* List *)

let list_test () =
  test "list"
    (Alcotest.list test_version)
    (Schematic.Schema.list_schema version_schema)
    [] "[]";
  test "list"
    (Alcotest.list test_version)
    (Schematic.Schema.list_schema version_schema)
    [ { major = 0; minor = 1; subminor = 2 } ]
    "[{\"major\": 0.000000, \"minor\": 1.000000, \"subminor\": 2.000000}].";
  test "list"
    (Alcotest.list test_version)
    (Schematic.Schema.list_schema version_schema)
    [
      { major = 0; minor = 1; subminor = 2 };
      { major = 3; minor = 4; subminor = 5 };
      { major = 6; minor = 7; subminor = 8 };
    ]
    "[{\"major\": 0, \"minor\": 1, \"subminor\": 2},{\"major\": 3, \"minor\": \
     4, \"subminor\": 5.00000},{\"major\": 6, \"minor\": 7, \"subminor\": 8}]."

(* Parametric *)

type 'a pair = 'a * 'a [@@deriving schema]

let parametric_type () =
  test "parametric_type"
    (Alcotest.pair test_version test_version)
    (pair_schema version_schema)
    (v1, v3)
    "[{\"major\": 0, \"minor\": 1, \"subminor\": 2},{\"major\": 6, \"minor\": \
     7, \"subminor\": 8}]."

(* Escape *)

type escape = {
  special_char : int; [@schematic.as "'char$"]
  keyword : string; [@schematic.as "type"]
}
[@@deriving schema]

let pp_escape fmt { special_char; keyword } =
  fprintf fmt "(%i, %s)" special_char keyword

let test_escape = Alcotest.testable pp_escape ( = )

let escape () =
  test "escape" test_escape escape_schema
    { special_char = 128; keyword = "coin" }
    "{\"'char$\": 128, \"type\": \"coin\"}"

(* Recursive *)

type recursive = {
  value : int;
  next : recursive option;
}
[@@deriving schema]

let pp_option pp fmt = function
  | None -> fprintf fmt "None"
  | Some v ->
    fprintf fmt "Some ";
    pp fmt v

let rec pp_recursive fmt { value; next } =
  fprintf fmt "(%i, %a)" value (pp_option pp_recursive) next

let test_recursive = Alcotest.testable pp_recursive ( = )

let recursive () =
  test "recursive" test_recursive recursive_schema
    { value = 0; next = Some { value = 1; next = None } }
    "{\"value\": 0, \"next\": {\"value\": 1}}"

type mutually_a = { next_b : mutually_b option }

and mutually_b = { next_a : mutually_a option } [@@deriving schema]

let rec pp_mutually_a fmt { next_b } = pp_option pp_mutally_b fmt next_b

and pp_mutally_b fmt { next_a } = pp_option pp_mutually_a fmt next_a

let test_mutually_a = Alcotest.testable pp_mutually_a ( = )

let mutually () =
  test "mutually recursive" test_mutually_a mutually_a_schema
    { next_b = Some { next_a = Some { next_b = None } } }
    "{\"next_b\": {\"next_a\": {}}}"

(* Inline *)

type inlined = {
  b : int;
  c : int;
}
[@@deriving schema]

type inline = {
  a : int;
  coin : inlined; [@schematic.inline]
  d : int;
}
[@@deriving schema]

let pp_inlined fmt { b; c } = fprintf fmt "(%i, %i)" b c

let pp_inline fmt { a; coin; d } =
  fprintf fmt "(%i, %a, %i)" a pp_inlined coin d

let test_inline = Alcotest.testable pp_inline ( = )

let inline () =
  test "inline" test_inline inline_schema
    { a = 0; coin = { b = 1; c = 2 }; d = 3 }
    "{\"a\": 0.000000, \"b\": 1.000000, \"c\": 2.000000, \"d\": 3.000000}"

(* Time *)

open Schematic.Schema.Schemas

type event = {
  start : Ptime.t;
  end_ : Ptime.t;
}

and event_local = {
  start_local : Ptime.local;
  end_local : Ptime.local;
}
[@@deriving schema]

let pp_event fmt e =
  let pp = Ptime.pp_rfc3339 () in
  Caml.Format.fprintf fmt "%a-%a" pp e.start pp e.end_

let pp_event_local fmt e =
  let pp fmt (t, tz) = Ptime.pp_rfc3339 ~tz_offset_s:tz () fmt t in
  Caml.Format.fprintf fmt "%a-%a" pp e.start_local pp e.end_local

let test_time = Alcotest.testable pp_event ( = )

let test_time_local = Alcotest.testable pp_event_local ( = )

let make_time s =
  Ptime.of_rfc3339 s |> Result.ok
  |> Option.value_exn ?here:None ?error:None ?message:None
  |> fun (t, _, _) -> t

let make_time_local s =
  Ptime.of_rfc3339 s |> Result.ok
  |> Option.value_exn ?here:None ?error:None ?message:None
  |> fun (t, tz, _) -> (t, Option.value_exn tz)

let time () =
  test "time" test_time event_schema
    {
      start = make_time "2020-05-08T23:48:34+02:00";
      end_ = make_time "2020-05-08T23:48:47+02:00";
    }
    {| {
         "start" : "2020-05-08T21:48:34-00:00",
         "end" : "2020-05-08T21:48:47-00:00"
       } |};
  test "time-local" test_time_local event_local_schema
    {
      start_local = make_time_local "2020-05-08T23:48:34+02:00";
      end_local = make_time_local "2020-05-08T23:48:47+02:00";
    }
    {| {
         "start_local" : "2020-05-08T23:48:34+02:00",
         "end_local" : "2020-05-08T23:48:47+02:00"
       } |}

(* JSON Schema metadata *)

module Metadata = struct
  type description = {
    field_a : int; [@schematic.properties { description = "Describe A" }]
    field_b : int; [@schematic.properties { description = "Describe B" }]
  }
  [@@deriving schema]

  let test schema expected =
    let expected =
      match Schematic.Json.of_string expected with
      | Result.Ok json -> json
      | Result.Error _ -> Alcotest.fail "invalid literal JSON"
    and effective = json_schema schema in
    Alcotest.check json "json-schema" expected effective

  let pp_description fmt { field_a; field_b } =
    fprintf fmt "(%i, %i)" field_a field_b

  let test_description = Alcotest.testable pp_description ( = )

  let description () =
    test description_schema
      {|{
          "additionalProperties" : false,
          "properties" :
          {
            "field_a" :
            {
              "$schema" : "https://json-schema.org/draft/2019-09/schema",
              "title" : "Field_a",
              "description" : "Describe A",
              "type" : "integer"
            },
            "field_b" :
            {
              "$schema" : "https://json-schema.org/draft/2019-09/schema",
              "title" : "Field_b",
              "description" : "Describe B",
              "type" : "integer"
            }
          },
          "$id" : "#description",
          "$schema" : "https://json-schema.org/draft/2019-09/schema",
          "type" : "object"
      }|}
end

(* Driver *)

let () =
  let open Alcotest in
  run "Schematic.Schema"
    [
      ( "atoms",
        [
          test_case "int" `Quick atom_int; test_case "string" `Quick atom_string;
        ] );
      ("record", [ test_case "record" `Quick record ]);
      ("tuple", [ test_case "tuple" `Quick tuple ]);
      ("variant", [ test_case "variant" `Quick variant ]);
      ("alias", [ test_case "alias" `Quick alias ]);
      ("list", [ test_case "list" `Quick list_test ]);
      ("parametric_type", [ test_case "parametric_type" `Quick parametric_type ]);
      ("escape", [ test_case "escape" `Quick escape ]);
      ( "recursive",
        [
          test_case "recursive" `Quick recursive;
          test_case "mutually recursive" `Quick mutually;
        ] );
      ("inline", [ test_case "inline" `Quick inline ]);
      ("time", [ test_case "time" `Quick time ]);
      ( "json-schema-metadata",
        [ test_case "description" `Quick Metadata.description ] );
      ("failure", [ test_case "failure" `Quick failure ]);
    ]

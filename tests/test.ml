open Base
open Poly
open Schematic.Schema.Schemas

let ( let* ) = Result.( >>= )

let ( let+ ) = Result.( >>| )

let root = Caml.Filename.dirname @@ Caml.Sys.executable_name

let json_schema schema =
  Schematic.Json.Schema.of_schema schema
  |> Schematic.Json.encode Schematic.Json.Schema.schema

let json = Alcotest.testable Schematic.Json.pp Schematic.Json.equal

let make_error ?(path = []) ?schema reason =
  Result.Error Schematic.Error.{ path; schema; reason }

let decoding_error =
  Alcotest.testable Schematic.Error.pp_decoding Schematic.Error.equal_decoding

let json_of_string s =
  match Schematic.Json.of_string s with
  | Result.Ok parsed -> parsed
  | Result.Error _ -> Alcotest.fail "invalid JSON expectation"

let test ?(validate = true) name test schema value j =
  let encoded = Schematic.Json.encode schema value in
  let j = json_of_string j in
  Alcotest.check json (Fmt.str "%s: encoded JSON" name) j encoded;
  let () =
    match Schematic.Json.decode schema j with
    | Result.Ok decoded ->
      Alcotest.check test (Fmt.str "%s: decoded value" name) value decoded
    | Result.Error e ->
      Alcotest.fail
        (Fmt.str "JSON decode error: %a" Schematic.Error.pp_decoding e)
  in
  if validate then
    let schema = json_schema schema in
    let cmd =
      Printf.sprintf "%s/validate-schema \"%s\" \"%s\"" root
        (String.escaped @@ Schematic.Json.to_string @@ schema)
        (String.escaped @@ Schematic.Json.to_string @@ encoded)
    in
    if Caml.Sys.command cmd <> 0 then
      Alcotest.check json (Fmt.str "%s: schema validation" name) encoded schema

(* Failure *)

let failure () =
  if
    try
      test "int atom" Alcotest.int int_schema 28 "29";
      true
    with
    | _ -> false
  then
    Alcotest.fail "failure test failed"

(* Atoms *)

let atom_int () = test "int atom" Alcotest.int int_schema 28 "28"

let atom_string () =
  test "int atom" Alcotest.string string_schema "trente trois"
    "\"trente trois\""

(* Option *)

let options () =
  let () =
    test "some int"
      Alcotest.(option int)
      (option_schema int_schema) (Some 28) "28"
  and () =
    test "none int" Alcotest.(option int) (option_schema int_schema) None "null"
  in
  ()

type nullable_option = { no : int option option } [@@deriving eq, schema, show]

(* Nullable option *)

let nullable_option = Alcotest.testable pp_nullable_option equal_nullable_option

let nullable_option () =
  let () =
    test "omitted" nullable_option nullable_option_schema { no = None } "{}"
  and () =
    test "null" nullable_option nullable_option_schema { no = Some None }
      {|{ "no" : null }|}
  and () =
    test "specified" nullable_option nullable_option_schema
      { no = Some (Some 74) } {|{ "no" : 74 }|}
  in
  ()

(* Default option*)

module Option = struct
  include Option

  let schema = Schematic.Schema.Schemas.option_schema

  let pp pp =
    Fmt.(option ~none:(const string "None") (const string "Some " ++ pp))
end

type default_option = { oo : int Option.t Option.t [@default None] }
[@@deriving eq, schema, show]

let default_option = Alcotest.(testable pp_default_option equal_default_option)

let default_option () =
  let () = test "none" default_option default_option_schema { oo = None } "{}"
  and () =
    test "some none" default_option default_option_schema { oo = Some None }
      {|{"oo": null}|}
  and () =
    test "some some" default_option default_option_schema
      { oo = Some (Some 877) } {|{"oo": 877}|}
  in
  ()

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

type version_tuple = int * int * int [@@deriving schema]

let version (major, minor, subminor) = { major; minor; subminor }

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
  | Aliased [@schematic.as "aktually"]
[@@deriving schema]

let pp_expectation fmt = function
  | Exact v -> pp_version fmt v
  | Range (l, r, i) -> fprintf fmt "(%a, %a, %i)" pp_version l pp_version r i
  | Nothing -> fprintf fmt "nothing"
  | Aliased -> fprintf fmt "aliased"

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
  test "variant" test_expectation expectation_schema Nothing "\"nothing\"";
  test "variant" test_expectation expectation_schema Aliased "\"aktually\""

(* Polymorphic variant *)

type day =
  [ `Mon
  | `Tue [@schematic.as "tuesday"]
  | `Wed of int
  | `Thu of bool * bool
  | `Fri
  | `Sat
  | `Sun
  ]
[@@deriving eq, schema, show]

let test_day = Alcotest.testable pp_day equal_day

let polymorphic_variant () =
  test "polymorphic variant" test_day day_schema `Mon {|"mon"|};
  test "polymorphic variant alias" test_day day_schema `Tue {|"tuesday"|};
  test "polymorphic variant payload" test_day day_schema (`Wed 160) {|160|};
  test "polymorphic variant composite payload" test_day day_schema
    (`Thu (true, false))
    {|[true, false]|}

(* List *)

let list_test () =
  test "list" (Alcotest.list test_version) (list_schema version_schema) [] "[]";
  test "list"
    (Alcotest.list test_version)
    (list_schema version_schema)
    [ { major = 0; minor = 1; subminor = 2 } ]
    "[{\"major\": 0.000000, \"minor\": 1.000000, \"subminor\": 2.000000}].";
  test "list"
    (Alcotest.list test_version)
    (list_schema version_schema)
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

(* Ignored *)

type ignored = {
  ignored : ignore;
  not_ignored : unit;
}
[@@deriving eq, schema, show]

let ignored () =
  let ignored = Alcotest.(testable pp_ignored equal_ignored) in
  test "missing ignored fields are accepted" ignored ignored_schema
    { ignored = (); not_ignored = () }
    "{\"not_ignored\": null}"

module Inline = struct
  type inlined = {
    b : int;
    c : int;
  }

  and inlined_variant =
    | Foo of inlined
    | Bar of {
        d : int;
        e : int;
      }

  and inline = {
    a : int;
    coin : inlined; [@schematic.inline]
    d : int;
    (* Check ignored fields can be inlined *)
    ignored : Schematic.Schema.Schemas.ignore; [@schematic.inline]
  }

  and variant_inline =
    | Ctor of {
        a : int;
        coin : inlined; [@schematic.inline]
        d : int;
        (* Check ignored fields can be inlined *)
        ignored : Schematic.Schema.Schemas.ignore; [@schematic.inline]
      }
    | Dummy of { dummy : int }

  and inline_variant = {
    x : int;
    pan : inlined_variant; [@schematic.inline]
  }

  and 'a inline_maybe = {
    maybe : bool;
    inline : 'a; [@schematic.inline_try]
  }
  [@@deriving eq, schema, show]

  let inline = Alcotest.testable pp_inline equal_inline

  let inlined = Alcotest.testable pp_inlined equal_inlined

  let variant_inline = Alcotest.testable pp_variant_inline equal_variant_inline

  let inline_variant = Alcotest.testable pp_inline_variant equal_inline_variant

  let inline_maybe a =
    Alcotest.(
      testable (pp_inline_maybe @@ pp a) (equal_inline_maybe @@ equal a))

  let descriptor_fields descriptor =
    match
      Schematic.Schema.(
        inline descriptor { f = Schematic.Schema.object_fields })
    with
    | Result.Ok sets -> List.concat sets
    | Result.Error e -> Alcotest.fail e

  let record () =
    let () =
      Alcotest.(
        check
          (list @@ list string)
          "the list of mandatory fields" [ [ "a"; "b"; "c"; "d" ] ]
        @@ descriptor_fields inline_schema.descriptor)
    in
    test "coding" inline inline_schema
      { a = 0; coin = { b = 1; c = 2 }; d = 3; ignored = () }
      "{\"a\": 0.000000, \"b\": 1.000000, \"c\": 2.000000, \"d\": 3.000000}"

  let variant_payload () =
    let () =
      Alcotest.(
        check
          (list @@ list string)
          "the list of mandatory fields"
          [ [ "a"; "b"; "c"; "d" ]; [ "dummy" ] ]
        @@ descriptor_fields variant_inline_schema.descriptor)
    in
    test "coding" variant_inline variant_inline_schema
      (Ctor { a = 0; coin = { b = 1; c = 2 }; d = 3; ignored = () })
      "{\"a\": 0.000000, \"b\": 1.000000, \"c\": 2.000000, \"d\": 3.000000}"

  let inline_variant () =
    let () =
      Alcotest.(
        check
          (list @@ list string)
          "the list of mandatory fields"
          [ [ "x"; "b"; "c" ]; [ "x"; "d"; "e" ] ]
        @@ descriptor_fields inline_variant_schema.descriptor)
    in
    let () =
      (* FIXME: json schema of inlined variant not handled yet *)
      test ~validate:false "coding of Foo" inline_variant inline_variant_schema
        { x = 362; pan = Foo { b = 363; c = 364 } }
        "{\"x\": 362.000000, \"b\": 363.000000, \"c\": 364.000000 }"
    in
    (* FIXME: json schema of inlined variant not handled yet *)
    test ~validate:false "coding of Bar" inline_variant inline_variant_schema
      { x = 369; pan = Bar { d = 370; e = 371 } }
      "{\"x\": 369.000000, \"d\": 370.000000, \"e\": 371.000000 }"

  let inline_try () =
    test "inlinable" (inline_maybe inlined)
      (inline_maybe_schema inlined_schema)
      { maybe = true; inline = { b = 411; c = 412 } }
      "{\"maybe\": true, \"b\": 411.000000, \"c\": 412.000000 }";
    test ~validate:false "not inlinable"
      (inline_maybe Alcotest.int)
      (inline_maybe_schema int_schema)
      { maybe = true; inline = 416 }
      "{\"maybe\": true, \"inline\": 416.000000 }"

  type 'a inline_parametric =
    | A of {
        value : int;
        param : 'a;
      }
    | B of {
        value : string;
        param : 'a;
      }
  [@@deriving eq, schema, show]

  type parametric = {
    inlined : Schematic.Schema.Schemas.ignore inline_parametric;
        [@schematic.inline]
        (* inlined : string option inline_parametric; [@schematic.inline] *)
  }
  [@@deriving eq, schema, show]

  (* let inline_parametric a =
   *   Alcotest.(
   *     testable
   *       (pp_inline_parametric @@ pp a)
   *       (equal_inline_parametric @@ equal a)) *)

  let parametric = Alcotest.testable pp_parametric equal_parametric

  let inline_parametric () =
    test ~validate:false "parametric inlinable" parametric parametric_schema
      { inlined = A { value = 457; param = () } }
      {|{"value": 457.000000 }|}
end

(* Time *)

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

(* Optional inline *)

type optional_inline2 = { d : int }

and optional_inline = {
  a : int;
  b : int;
  inline2 : optional_inline2; [@schematic.inline]
  missing : int option;
}

and optional_inline_wrapper = {
  inline : optional_inline option; [@schematic.inline]
  c : int;
}
[@@deriving schema, show]

let optional_inline () =
  let t = Alcotest.testable pp_optional_inline_wrapper ( = ) in
  test "missing optional inline" t optional_inline_wrapper_schema
    { inline = None; c = 377 } {|{"c":377}|};
  test "filled optional inline" t optional_inline_wrapper_schema
    {
      inline = Some { a = 375; b = 376; inline2 = { d = 378 }; missing = None };
      c = 377;
    }
    {|{"a":375,"b":376,"d":378,"c":377}|}

type keyed =
  | Paragraph_ of { content : string } [@schematic.as "paragraph"]
  | Header of {
      content : string;
      level : int;
    }
[@@schematic.name "keyed"] (* Check multiple attributes work *)
[@@schematic.union_key "type"]
[@@deriving eq, schema, show]

let keyed () =
  let tester = Alcotest.(testable pp_keyed equal_keyed) in
  (* FIXME: we should validate but our json schema does not support keyed unions
     yet *)
  let () =
    test ~validate:false "paragraph" tester keyed_schema
      (Paragraph_ { content = "text" })
      {| { "content": "text", "type": "paragraph" } |}
  and () =
    test ~validate:false "header" tester keyed_schema
      (Header { content = "text"; level = 411 })
      {| { "content": "text", "level": 411, "type": "header" } |}
  and () =
    let json = json_of_string {| { "content": "text", "level": 411 } |} in
    Alcotest.(
      check
        (result tester decoding_error)
        "values are rejected if the key is missing"
        (make_error ~schema:"keyed" {|missing key field "type"|})
        (Schematic.Json.decode keyed_schema json))
  and () =
    let json =
      json_of_string {| { "content": "text", "level": 411, "type": "fumble" } |}
    in
    Alcotest.(
      check
        (result tester decoding_error)
        "values are rejected if the key not recognized"
        (make_error ~schema:"keyed" {|invalid key field "type" = "fumble"|})
        (Schematic.Json.decode keyed_schema json))
  and () =
    let json = json_of_string {| { "type": "header", "content": "text" } |} in
    Alcotest.(
      check
        (result tester decoding_error)
        "errors are specific to the matched case schema"
        (make_error ~schema:"keyed" {|missing required field "level"|})
        (Schematic.Json.decode keyed_schema json))
  in
  ()

let map_error () =
  let schema =
    Schematic.Schema.make (Map ((fun s -> Result.fail s), Fn.id, String))
  in
  Alcotest.(
    check
      (result string decoding_error)
      "errors from Map descriptor decoding are forwarded properly"
      (make_error "whatever")
      (Schematic.Json.decode schema (`String "whatever")))

(* JSON Schema metadata *)

module Metadata = struct
  let test_schema schema expected =
    let expected =
      match Schematic.Json.of_string expected with
      | Result.Ok json -> json
      | Result.Error _ -> Alcotest.fail "invalid literal JSON"
    and effective = json_schema schema in
    Alcotest.check json "json-schema" expected effective

  type description = {
    field_a : int; [@schematic.properties { description = "Describe A" }]
    field_b : int; [@schematic.properties { description = "Describe B" }]
  }
  [@@deriving schema]

  let description () =
    test_schema description_schema
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

  type default = {
    a : int; [@schematic.default 358]
    b : string; [@schematic.default "359"]
  }
  [@@deriving schema]

  let pp_default fmt { a; b } = fprintf fmt "(%i, %s)" a b

  let default () =
    test_schema default_schema
      {|{
          "additionalProperties" : false,
          "properties" :
          {
            "a" :
            {
              "$schema" : "https://json-schema.org/draft/2019-09/schema",
              "title" : "A",
              "default": 358,
              "type" : "integer"
            },
            "b" :
            {
              "$schema" : "https://json-schema.org/draft/2019-09/schema",
              "title" : "B",
              "default": "359",
              "type" : "string"
            }
          },
          "$id" : "#default",
          "$schema" : "https://json-schema.org/draft/2019-09/schema",
          "type" : "object"
       }|};
    let tester = Alcotest.(testable pp_default ( = )) in
    test "default-int" tester default_schema { a = 358; b = "---" }
      {| { "b": "---" } |};
    test "default-string" tester default_schema { a = 359; b = "359" }
      {| { "a": 359 } |};
    test "default-both" tester default_schema { a = 358; b = "359" } {| {} |};
    test "default-none" tester default_schema { a = 395; b = "---" }
      {| { "a": 395, "b": "---" } |}
end

module Comparison = struct
  open Alcotest

  let check name schema e l r =
    check int name e (Schematic.Schema.compare schema l r)

  let bool () =
    let check = check "bool" bool_schema in
    let () = check 0 true true in
    let () = check 0 false false in
    let () = check 1 true false in
    let () = check (-1) false true in
    ()

  let int () =
    let check = check "int" int_schema in
    let () = check 0 0 0 in
    let () = check (-1) 0 1 in
    let () = check 1 1 0 in
    ()

  let string () =
    let check = check "string" string_schema in
    let () = check 0 "a" "a" in
    let () = check (-1) "a" "b" in
    let () = check 1 "b" "a" in
    let () = check 1 "aa" "a" in
    let () = check (-1) "a" "aa" in
    ()

  let versions check f =
    let v012 = (0, 1, 2)
    and v013 = (0, 1, 3)
    and v022 = (0, 2, 2) in
    let tests =
      [
        (0, v012, v012);
        (-1, v012, v013);
        (1, v013, v012);
        (-1, v012, v022);
        (1, v022, v012);
      ]
    in
    List.iter ~f:(fun (exp, l, r) -> check exp (f l) (f r)) tests

  let record () =
    let check = check "version" version_schema in
    versions check version

  let tuple () =
    let check = check "version" version_tuple_schema in
    versions check Fn.id

  let variant () =
    let check = check "variant" expectation_schema
    and v = { major = 1; minor = 2; subminor = 3 } in
    let () = check 0 (Exact v) (Exact v)
    and () = check (-1) (Exact v) Nothing
    and () = check 1 Nothing (Exact v) in
    ()
end

module Error_messages = struct
  let test name schema str e =
    let res =
      let* json =
        Schematic.Json.of_string str
        |> Result.map_error ~f:Schematic.Error.of_parsing
      in
      let+ _ =
        Schematic.Json.decode schema json
        |> Result.map_error ~f:(Schematic.Error.of_decoding `JSON)
      in
      ()
    in
    Alcotest.(
      check (result unit string) name (Result.Error e)
        (Result.map_error ~f:Schematic.Error.to_string res))

  let missing () =
    let () =
      test "missing fields are reported" recursive_schema
        {|{"value":0,"next":{"value":1,"next":{}}}|}
        {|path next.next: schema recursive: missing required field "value"|}
    and () =
      test "root error display no path" recursive_schema {|{}|}
        {|schema recursive: missing required field "value"|}
    in
    ()

  let type_error () =
    test "missing" recursive_schema
      {|{"value":0,"next":{"value":1,"next":{"value":"nope"}}}|}
      {|path next.next.value: expected integer, got string|}
end

(* Phantom types *)

type _ id = string [@@deriving schema]

type some_id = int id [@@deriving schema]

type identified = { id : identified id [@schematic.schema id_schema ()] }
[@@deriving schema]

type t = int [@@deriving schema]

let root_schema = schema

module Names = struct
  type foo = int [@@deriving schema]

  type t = int [@@deriving schema]

  let deduced () =
    Alcotest.(
      check (option string) "schema name is deduced from type name" (Some "foo")
        foo_schema.id);
    Alcotest.(
      check (option string)
        "schema name is deduced from type module name when type is \"t\""
        (Some "names") schema.id);
    Alcotest.(
      check (option string)
        "schema name is deduced from file name when type is \"t\"" (Some "test")
        root_schema.id)

  type 'a result =
    | Ok of 'a
    | Error
  [@@deriving schema]

  let parametric () =
    Alcotest.(
      check (option string) "schema name is deduced from type name"
        (Some "foo-result") (result_schema foo_schema).id);
    Alcotest.(
      check (option string)
        "schema name is deduced from type module name when type is \"t\""
        (Some "names-result") (result_schema schema).id)
end

[@@@warning "-30"]

type collision_1 = {
  collision_a : int;
  collision_b : string;
}

and 't collision_2 = {
  collision_a : int; [@warning "-30"]
  collision_b : 't;
}
[@@deriving eq, schema, show]

[@@@warning "+30"]

let collision_1 = Alcotest.testable pp_collision_1 equal_collision_1

let collision_2 =
  Alcotest.testable (pp_collision_2 Fmt.string) (equal_collision_2 String.equal)

let collision () =
  let () =
    test "collision_1" collision_1 collision_1_schema
      ({ collision_a = 725; collision_b = "725" } : collision_1)
      {|{"collision_a": 725, "collision_b": "725"}|}
  and () =
    test "collision_2" collision_2
      (collision_2_schema Schematic.Schema.Schemas.string_schema)
      ({ collision_a = 725; collision_b = "725" } : string collision_2)
      {|{"collision_a": 725, "collision_b": "725"}|}
  in
  ()

(* Driver *)

let () =
  let open Alcotest in
  run "Schematic.Schema"
    [
      ( "coding",
        [
          test_case "int" `Quick atom_int;
          test_case "string" `Quick atom_string;
          test_case "option" `Quick options;
          test_case "nullable option" `Quick nullable_option;
          test_case "default option" `Quick default_option;
          test_case "record" `Quick record;
          test_case "tuple" `Quick tuple;
          test_case "variant" `Quick variant;
          test_case "polymorphic variant" `Quick polymorphic_variant;
          test_case "alias" `Quick alias;
          test_case "list" `Quick list_test;
          test_case "parametric_type" `Quick parametric_type;
          test_case "escape" `Quick escape;
          test_case "recursive" `Quick recursive;
          test_case "mutually recursive" `Quick mutually;
          test_case "ignored" `Quick ignored;
          test_case "inline-record" `Quick Inline.record;
          test_case "inline-variant-payload" `Quick Inline.variant_payload;
          test_case "inline-variant" `Quick Inline.inline_variant;
          test_case "inline-try" `Quick Inline.inline_try;
          test_case "inline-parametric" `Quick Inline.inline_parametric;
          test_case "time" `Quick time;
          test_case "json-schema-metadata-description" `Quick
            Metadata.description;
          test_case "json-schema-metadata-default" `Quick Metadata.default;
          test_case "failure" `Quick failure;
          test_case "optional-inline" `Quick optional_inline;
          test_case "keyed" `Quick keyed;
          test_case "map-error" `Quick map_error;
          test_case "collisions" `Quick collision;
        ] );
      ( "comparison",
        [
          test_case "bool" `Quick Comparison.bool;
          test_case "int" `Quick Comparison.int;
          test_case "string" `Quick Comparison.string;
          test_case "record" `Quick Comparison.record;
          test_case "tuple" `Quick Comparison.tuple;
          test_case "variant" `Quick Comparison.variant;
        ] );
      ( "error-messages",
        [
          test_case "missing" `Quick Error_messages.missing;
          test_case "type error" `Quick Error_messages.type_error;
        ] );
      ( "names",
        [
          test_case "deduced names" `Quick Names.deduced;
          test_case "parametric schemas names" `Quick Names.parametric;
        ] );
    ]

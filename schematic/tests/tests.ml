open Base
open Poly
open Schematic.Schemas

module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

let ( let* ) = Result.( >>= )
let ( let+ ) = Result.( >>| )

let json_schema schema =
  Schematic.Json.Schema.of_schema schema
  |> Schematic.Json.encode Schematic.Json.Schema.schema

let make_error ?(path = []) ?schema reason =
  Result.Error Schematic.Error.{ path; schema; reason }

let decoding_error =
  Alcotest.testable Schematic.Error.pp_decoding Schematic.Error.equal_decoding

let json = Alcotest.testable Schematic.Json.pp Schematic.Json.equal

type t = int [@@deriving schema]

let root_schema = schema

type test_description =
  | Test : {
      here : Lexing.position;
      decoded : 'a option;
      name : string;
      value_testable : 'a Alcotest.testable;
      schema : 'a Schema.t;
      value : 'a;
      expected : Ezjsonm.value;
      kotlin_class_name : string option;
          (** TODO Remove this field when Kotlin support is complete *)
      is_kotlin_roundtrip_enabled : bool;
    }
      -> test_description

type test_case = {
  name : string;
  tests : test_description list;
  speed : Alcotest.speed_level;
  additional_test : (unit -> unit) option;
}

type test_group = {
  name : string;
  test_cases : test_case list;
}

module Make (Coding : sig
  include Schematic.Coding.S

  val t : t Alcotest.testable
  val extra_test : test_description -> t -> unit
  val of_json : json -> t option
  val local_time_support : bool
end) =
struct
  let test ~here ?decoded name value_testable schema value expected =
    let encoded = Coding.encode schema value in
    let () =
      match Coding.of_json expected with
      | Some expected ->
        Alcotest.check ~here Coding.t
          (Fmt.str "%s: encoded JSON" name)
          expected encoded
      | None -> ()
    in
    match Coding.decode schema encoded with
    | Result.Ok d ->
      Alcotest.check ~here value_testable
        (Fmt.str "%s: decoded value (encoded JSON: %a)" name
           (Alcotest.pp Coding.t) encoded)
        (Option.value ~default:value decoded)
        d
    | Result.Error e ->
      Alcotest.fail
        (Fmt.str "JSON decode error: %a" Schematic.Error.pp_decoding e)

  let test2
      (Test
         {
           here;
           decoded;
           name;
           value_testable;
           schema;
           value;
           expected;
           kotlin_class_name = _;
           is_kotlin_roundtrip_enabled = _;
         } as test_descripton) =
    let encoded = Coding.encode schema value in
    let () =
      Fmt.epr "Running test2 for schema %a@." Schematic.Schema.pp schema
    in
    let () = test ~here ?decoded name value_testable schema value expected in
    Coding.extra_test test_descripton encoded

  module Option = struct
    include Option

    let schema_versioned = Schematic.Schemas.option_schema_versioned

    let pp pp =
      Fmt.(option ~none:(const string "None") (const string "Some " ++ pp))
  end

  (* Failure *)

  let failure () =
    if Option.is_some (Coding.of_json [%ezjsonm 29]) then
      if
        try
          test ~here:[%here] "int atom" Alcotest.int int_schema 28 [%ezjsonm 29];
          true
        with _ -> false
      then Alcotest.fail "failure test failed"

  (* Atoms *)

  let atom_int =
    {
      name = "int";
      speed = `Quick;
      additional_test = None;
      tests =
        [
          Test
            {
              name = "int atom";
              here = [%here];
              decoded = None;
              value_testable = Alcotest.int;
              schema = int_schema;
              value = 28;
              expected = [%ezjsonm 28];
              kotlin_class_name = Some "java.lang.Integer";
              is_kotlin_roundtrip_enabled = true;
            };
        ];
    }

  let atom_string =
    {
      name = "string";
      speed = `Quick;
      additional_test = None;
      tests =
        [
          Test
            {
              name = "string atom";
              here = [%here];
              decoded = None;
              value_testable = Alcotest.string;
              schema = string_schema;
              value = "trente trois";
              expected = [%ezjsonm "trente trois"];
              kotlin_class_name = Some "java.lang.String";
              is_kotlin_roundtrip_enabled = true;
            };
        ];
    }

  (* Optionals *)

  module Optionals = struct
    let options =
      let mk_test name value expected is_kotlin_roundtrip_enabled =
        Test
          {
            name;
            here = [%here];
            decoded = None;
            value_testable = Alcotest.(option int);
            schema = option_schema int_schema;
            value;
            expected;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled;
          }
      in
      {
        name = "option";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            mk_test "some int" (Some 28) [%ezjsonm 28] false;
            mk_test "none int" None [%ezjsonm None] false;
          ];
      }

    (* Nullable option *)

    type nullable_option = { no : int option option }
    [@@deriving eq, schema, show]

    let nullable_option =
      Alcotest.testable pp_nullable_option equal_nullable_option

    let nullable_option =
      let mk_test name value expected is_kotlin_roundtrip_enabled =
        Test
          {
            name;
            here = [%here];
            decoded = None;
            value_testable = nullable_option;
            schema = nullable_option_schema;
            value;
            expected;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled;
          }
      in
      {
        name = "nullable option";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            mk_test "omitted" { no = None } (`O []) false;
            mk_test "null" { no = Some None } [%ezjsonm { no = None }] true;
            mk_test "specified" { no = Some (Some 74) } [%ezjsonm { no = 74 }]
              true;
          ];
      }

    (* Default option*)

    type default_option = { oo : int Option.t Option.t [@default None] }
    [@@deriving eq, schema, show]

    let default_option =
      Alcotest.(testable pp_default_option equal_default_option)

    let default_option =
      let mk_test name value expected is_kotlin_roundtrip_enabled =
        Test
          {
            name;
            here = [%here];
            decoded = None;
            value_testable = default_option;
            schema = default_option_schema;
            value;
            expected;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled;
          }
      in
      {
        name = "default option";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            mk_test "none" { oo = None } (`O []) false;
            mk_test "some none" { oo = Some None } [%ezjsonm { oo = None }] true;
            mk_test "some some" { oo = Some (Some 877) } [%ezjsonm { oo = 877 }]
              true;
          ];
      }

    (* Disabled optionals *)

    type disabled_optionals = {
      a : int option; [@schematic.default_opt None]
      b : int list; [@default []] [@schematic.default_opt None]
    }
    [@@deriving eq, schema, show]

    let disabled_optionals =
      let disabled_optionals =
        Alcotest.testable pp_disabled_optionals equal_disabled_optionals
      in
      {
        name = "disabled optionals";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            Test
              {
                name = "disabled optionals";
                here = [%here];
                decoded = None;
                value_testable = disabled_optionals;
                schema = disabled_optionals_schema;
                value = { a = None; b = [] };
                expected = [%ezjsonm { a = None; b = [] }];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled = false;
              };
          ];
      }
  end

  (* Record *)

  type version = {
    major : int;
    minor : int;
    subminor : int;
  }
  [@@deriving schema]

  let pp_version fmt { major; minor; subminor } =
    Fmt.pf fmt "%i.%i.%i" major minor subminor

  let test_version = Alcotest.testable pp_version ( = )

  let v1 = { major = 0; minor = 1; subminor = 2 }
  and v2 = { major = 3; minor = 4; subminor = 5 }
  and v3 = { major = 6; minor = 7; subminor = 8 }

  let record =
    {
      name = "record";
      speed = `Quick;
      additional_test = None;
      tests =
        [
          Test
            {
              name = "record";
              here = [%here];
              decoded = None;
              value_testable = test_version;
              schema = version_schema;
              value = v1;
              expected = [%ezjsonm { major = 0; minor = 1; subminor = 2 }];
              kotlin_class_name = None;
              is_kotlin_roundtrip_enabled = true;
            };
        ];
    }

  (* Alias *)

  type version_alias = version [@@deriving schema]

  let alias () =
    test ~here:[%here] "record" test_version version_alias_schema v2
      [%ezjsonm { major = 3; minor = 4; subminor = 5 }]

  (* Tuple *)

  type version_tuple = int * int * int [@@deriving schema]

  let version (major, minor, subminor) = { major; minor; subminor }

  type range = version * version * int [@@deriving schema]

  let pp_range fmt (l, r, i) =
    Fmt.pf fmt "(%a, %a, %i)" pp_version l pp_version r i

  let test_range = Alcotest.testable pp_range ( = )

  let tuple =
    {
      name = "tuple";
      speed = `Quick;
      additional_test = None;
      tests =
        [
          Test
            {
              name = "tuple";
              here = [%here];
              decoded = None;
              value_testable = test_range;
              schema = range_schema;
              value = (v1, v2, 83);
              expected =
                [%ezjsonm
                  [
                    { major = 0; minor = 1; subminor = 2 };
                    { major = 3; minor = 4; subminor = 5 };
                    83;
                  ]];
              kotlin_class_name = None;
              (* Type aliases are not supported for reflection *)
              is_kotlin_roundtrip_enabled = false;
            };
        ];
    }

  (* Variant *)

  type expectation =
    | Exact of version
    | Range of range
    | Nothing
    | Aliased [@schematic.as "aktually"] [@schematic.aliases [ "mrmask" ]]
  [@@deriving schema, show]

  let test_expectation = Alcotest.testable pp_expectation ( = )

  module Variant = struct
    (** Kotlin roundtrip disabled because *)
    let variant =
      let mk_test name value expected =
        Test
          {
            name;
            here = [%here];
            decoded = None;
            value_testable = test_expectation;
            schema = expectation_schema;
            value;
            expected;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled = false;
            (* Nested unions (sealed interfaces) not yet supported. The
               [aktually] variant is itself a union of ["aktually"] and
               ["mrmask"]. *)
          }
      in
      {
        name = "variant";
        speed = `Quick;
        tests =
          [
            mk_test "variant"
              (Exact { major = 1; minor = 2; subminor = 3 })
              [%ezjsonm { major = 1; minor = 2; subminor = 3 }];
            mk_test "variant"
              (Range
                 ( { major = 1; minor = 2; subminor = 3 },
                   { major = 4; minor = 5; subminor = 6 },
                   108 ))
              [%ezjsonm
                [
                  { major = 1; minor = 2; subminor = 3 };
                  { major = 4; minor = 5; subminor = 6 };
                  108;
                ]];
            mk_test "variant" Nothing [%ezjsonm "nothing"];
            mk_test "variant" Aliased [%ezjsonm "aktually"];
          ];
        additional_test =
          Some
            (fun () ->
              Alcotest.(
                check ~here:[%here] (result test_expectation decoding_error))
                "aliased variant field" (Result.Ok Aliased)
              @@ Schematic.Json.decode expectation_schema [%ezjsonm "mrmask"]);
      }

    type singleton = Single [@@deriving eq, schema, show]

    let singleton = Alcotest.testable pp_singleton equal_singleton

    let singleton =
      {
        name = "singleton variant";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            Test
              {
                name = "singleton variant";
                here = [%here];
                decoded = None;
                value_testable = singleton;
                schema = singleton_schema;
                value = Single;
                expected = [%ezjsonm "single"];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled = true;
              };
          ];
      }

    type computed_payload =
      | Computed of {
          x : int;
          y : int;
        } [@schematic.computed { z : int = x + y }]
    [@@deriving eq, schema, show]

    let computed_payload =
      Alcotest.testable pp_computed_payload equal_computed_payload

    let computed =
      {
        name = "computed variant field";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            Test
              {
                name = "variant payload with computed field";
                here = [%here];
                decoded = None;
                value_testable = computed_payload;
                schema = computed_payload_schema;
                value = Computed { x = 239; y = 114 };
                expected = [%ezjsonm { x = 239; y = 114; z = 353 }];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled = true;
              };
          ];
      }

    let no_match =
      let error =
        Schematic.Json.decode expectation_schema
          [%ezjsonm { invalid = "payload" }]
      in
      {
        name = "unmatched variant";
        speed = `Quick;
        tests = [];
        additional_test =
          Some
            (fun () ->
              Alcotest.(
                check ~here:[%here] (result test_expectation decoding_error))
                "invalid variant decoding"
                (Error
                   {
                     path = [];
                     schema = Some "expectation";
                     reason =
                       {|no union case was matched for {"invalid": "payload"}|};
                   })
                error);
      }

    type null_object =
      | Object of { x : int }
      | Null of unit
    [@@deriving eq, schema, show]

    (* Used to fail on Jsoo with:

       TypeError: Cannot convert undefined or null to object *)
    let null_object =
      {
        name = "null object";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            Test
              {
                name = "null object";
                here = [%here];
                decoded = None;
                value_testable =
                  Alcotest.testable pp_null_object equal_null_object;
                schema = null_object_schema;
                value = Null ();
                expected = [%ezjsonm None];
                kotlin_class_name = None;
                (* TODO fix this bug *)
                is_kotlin_roundtrip_enabled = false;
              };
          ];
      }
  end

  (* Polymorphic variant *)

  module Polymorphic_variant = struct
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

    let payload =
      let mk_test name value expected is_kotlin_roundtrip_enabled =
        Test
          {
            name;
            here = [%here];
            decoded = None;
            value_testable = test_day;
            schema = day_schema;
            value;
            expected;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled;
          }
      in
      {
        name = "polymorphic variant";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            mk_test "polymorphic variant" `Mon [%ezjsonm "mon"] true;
            mk_test "polymorphic variant alias" `Tue [%ezjsonm "tuesday"] true;
            mk_test "polymorphic variant payload" (`Wed 160) [%ezjsonm 160] true;
            mk_test "polymorphic variant composite payload"
              (`Thu (true, false))
              [%ezjsonm [ true; false ]] true;
          ];
      }

    type keyed =
      [ `Left
      | `Right
      | `Center of pl
      ]
    [@@schematic.union_key "position"]

    and pl = { stable : bool } [@@deriving eq, schema, show]

    let test_keyed = Alcotest.testable pp_keyed equal_keyed

    let keyed =
      let mk_test name value expected is_kotlin_roundtrip_enabled =
        Test
          {
            name;
            here = [%here];
            decoded = None;
            value_testable = test_keyed;
            schema = keyed_schema;
            value;
            expected;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled;
          }
      in
      {
        name = "keyed polymorphic variant";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            mk_test "keyed polymorphic variant" `Left
              [%ezjsonm { position = "left" }] true;
            mk_test "keyed polymorphic variant"
              (`Center { stable = false })
              [%ezjsonm { position = "center"; stable = false }]
              true;
          ];
      }
  end

  (* List *)

  module List = struct
    let objects () =
      test ~here:[%here] "list"
        (Alcotest.list test_version)
        (list_schema version_schema)
        [] [%ezjsonm []];
      test ~here:[%here] "list"
        (Alcotest.list test_version)
        (list_schema version_schema)
        [ { major = 0; minor = 1; subminor = 2 } ]
        [%ezjsonm [ { major = 0; minor = 1; subminor = 2 } ]];
      test ~here:[%here] "list"
        (Alcotest.list test_version)
        (list_schema version_schema)
        [
          { major = 0; minor = 1; subminor = 2 };
          { major = 3; minor = 4; subminor = 5 };
          { major = 6; minor = 7; subminor = 8 };
        ]
        [%ezjsonm
          [
            { major = 0; minor = 1; subminor = 2 };
            { major = 3; minor = 4; subminor = 5 };
            { major = 6; minor = 7; subminor = 8 };
          ]]

    let option () =
      test ~here:[%here] "list"
        Alcotest.(list (option int))
        (list_schema (option_schema int_schema))
        [] [%ezjsonm []];
      test ~here:[%here] "list"
        Alcotest.(list (option int))
        (list_schema (option_schema int_schema))
        [ Some 1; Some 2; Some 3 ] [%ezjsonm [ 1; 2; 3 ]];
      test ~here:[%here] "list"
        Alcotest.(list (option int))
        (list_schema (option_schema int_schema))
        [ None; None; None ] [%ezjsonm [ None; None; None ]];
      test ~here:[%here] "list"
        Alcotest.(list (option int))
        (list_schema (option_schema int_schema))
        [ Some 1; None; Some 3 ] [%ezjsonm [ 1; None; 3 ]]
  end

  (* Parametric *)

  module Parametric = struct
    type 'a pair = 'a * 'a [@@deriving schema]

    let simple () =
      test ~here:[%here] "parametric_type"
        (Alcotest.pair test_version test_version)
        (pair_schema version_schema)
        (v1, v3)
        [%ezjsonm
          [
            { major = 0; minor = 1; subminor = 2 };
            { major = 6; minor = 7; subminor = 8 };
          ]]

    type _ anonymous = int [@@deriving schema]

    let anonymous () =
      test ~here:[%here] "anonymous parameter" Alcotest.int
        (anonymous_schema version_schema)
        345 [%ezjsonm 345]
  end

  (* GADT *)

  module Gadt = struct
    type _ simple = Int : int simple [@@deriving eq, schema, show]

    let simple = Alcotest.testable pp_simple equal_simple

    let simple () =
      test ~here:[%here] "simple GADT" simple
        (simple_schema version_schema)
        Int [%ezjsonm "int"]

    type _ complex =
      | Int : int complex [@schematic.gadt int]
      | String : string -> string complex [@schematic.gadt string]
    [@@deriving schema ~gadt:[ int; string ]]

    let pp_complex (type t) f (complex : t complex) =
      match complex with
      | Int -> Fmt.string f "Int"
      | String s -> Fmt.pf f "String %S" s

    let equal_complex (type l r) (l : l complex) (r : r complex) =
      match (l, r) with
      | Int, Int -> true
      | String l, String r -> String.equal l r
      | _ -> false

    let complex () = Alcotest.testable pp_complex equal_complex

    let complex () =
      let () =
        test ~here:[%here] "complex GADT" (complex ())
          (int_complex_schema int_schema)
          Int [%ezjsonm "int"]
      in
      test ~here:[%here] "complex GADT" (complex ())
        (string_complex_schema string_schema)
        (String "386") [%ezjsonm "386"]
  end

  (* Escape *)

  type escape = {
    special_char : int; [@schematic.as "'char$"]
    keyword : string; [@schematic.as "type"]
  }
  [@@deriving schema]

  let pp_escape fmt { special_char; keyword } =
    Fmt.pf fmt "(%i, %s)" special_char keyword

  let test_escape = Alcotest.testable pp_escape ( = )

  let escape () =
    test ~here:[%here] "escape" test_escape escape_schema
      { special_char = 128; keyword = "coin" }
      (`O [ ("'char$", `Float 128.); ("type", `String "coin") ])

  (* Recursive *)

  module Recursive = struct
    type t = {
      value : int;
      next : t option;
    }
    [@@schematic.name "recursive"] [@@deriving schema]

    let rec pp_recursive fmt { value; next } =
      Fmt.pf fmt "(%i, %a)" value (Fmt.option pp_recursive) next

    let test_recursive = Alcotest.testable pp_recursive ( = )

    let plain () =
      test ~here:[%here] "recursive" test_recursive schema
        { value = 0; next = Some { value = 1; next = None } }
        [%ezjsonm { value = 0; next = { value = 1 } }]

    (* Used to check the lazy fixpoint schema is evaluated only once *)
    type count = unit [@@deriving eq, show]

    let count = ref 0

    let count_schema_versioned _ =
      count := !count + 1;
      unit_schema

    type 'a parametric = {
      count : count; [@schematic.omit ()]
      next : 'a parametric option;
      value : 'a;
    }
    [@@schematic.name "recursive-parametric"]
    [@@deriving eq, schema ~fixpoint, show]

    let parametric t =
      Alcotest.testable
        (pp_parametric (Alcotest.pp t))
        (equal_parametric (Alcotest.equal t))

    let parametric () =
      let () = Alcotest.(check ~here:[%here] int "first count" 0 !count) in
      let schema = parametric_schema int_schema in
      let () = Alcotest.(check ~here:[%here] int "second count" 1 !count) in
      let _schema = parametric_schema int_schema in
      let () = Alcotest.(check ~here:[%here] int "third count" 1 !count) in
      let () =
        test ~here:[%here] "recursive" (parametric Alcotest.int) schema
          {
            count = ();
            value = 0;
            next = Some { count = (); value = 1; next = None };
          }
          [%ezjsonm { value = 0; next = { value = 1 } }]
      in
      Alcotest.(check ~here:[%here] int "fourth count" 1 !count)
  end

  type mutually_a = { next_b : mutually_b option }
  and mutually_b = { next_a : mutually_a option } [@@deriving schema]

  let rec pp_mutually_a fmt { next_b } = Fmt.option pp_mutally_b fmt next_b
  and pp_mutally_b fmt { next_a } = Fmt.option pp_mutually_a fmt next_a

  let test_mutually_a = Alcotest.testable pp_mutually_a ( = )

  let mutually () =
    test ~here:[%here] "mutually recursive" test_mutually_a mutually_a_schema
      { next_b = Some { next_a = Some { next_b = None } } }
      [%ezjsonm { next_b = { next_a = [%aq `O []] } }]

  (* Ignored *)

  type ignored = {
    ignored : ignore;
    not_ignored : unit;
  }
  [@@deriving eq, schema, show]

  let ignored () =
    let ignored = Alcotest.(testable pp_ignored equal_ignored) in
    test ~here:[%here] "missing ignored fields are accepted" ignored
      ignored_schema
      { ignored = (); not_ignored = () }
      [%ezjsonm { not_ignored = None }]

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
      ignored : Schematic.Schemas.ignore; [@schematic.inline]
    }

    and variant_inline =
      | Ctor of {
          a : int;
          coin : inlined; [@schematic.inline]
          d : int;
          (* Check ignored fields can be inlined *)
          ignored : Schematic.Schemas.ignore; [@schematic.inline]
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

    let variant_inline =
      Alcotest.testable pp_variant_inline equal_variant_inline

    let inline_variant =
      Alcotest.testable pp_inline_variant equal_inline_variant

    let inline_maybe a =
      Alcotest.(
        testable (pp_inline_maybe @@ pp a) (equal_inline_maybe @@ equal a))

    let descriptor_fields descriptor =
      match
        Schematic.Schema.(
          Inline.schema descriptor
            { f = (fun { name; _ } -> name); filter = None })
      with
      | Result.Ok sets -> sets
      | Result.Error e -> Alcotest.fail e

    let record () =
      let () =
        Alcotest.(
          check ~here:[%here]
            (list @@ list string)
            "the list of mandatory fields"
            [ [ "a"; "b"; "c"; "d" ] ]
          @@ descriptor_fields inline_schema.descriptor)
      in
      test ~here:[%here] "coding" inline inline_schema
        { a = 0; coin = { b = 1; c = 2 }; d = 3; ignored = () }
        [%ezjsonm { a = 0; b = 1; c = 2; d = 3 }]

    let variant_payload () =
      let () =
        Alcotest.(
          check ~here:[%here]
            (list @@ list string)
            "the list of mandatory fields"
            [ [ "a"; "b"; "c"; "d" ]; [ "dummy" ] ]
          @@ descriptor_fields variant_inline_schema.descriptor)
      in
      test ~here:[%here] "coding" variant_inline variant_inline_schema
        (Ctor { a = 0; coin = { b = 1; c = 2 }; d = 3; ignored = () })
        [%ezjsonm { a = 0; b = 1; c = 2; d = 3 }]

    let inline_variant () =
      let () =
        Alcotest.(
          check ~here:[%here]
            (list @@ list string)
            "the list of mandatory fields"
            [ [ "x"; "b"; "c" ]; [ "x"; "d"; "e" ] ]
          @@ descriptor_fields inline_variant_schema.descriptor)
      in
      let () =
        test ~here:[%here] "coding of Foo" inline_variant inline_variant_schema
          { x = 362; pan = Foo { b = 363; c = 364 } }
          [%ezjsonm { x = 362; b = 363; c = 364 }]
      in
      test ~here:[%here] "coding of Bar" inline_variant inline_variant_schema
        { x = 369; pan = Bar { d = 370; e = 371 } }
        [%ezjsonm { x = 369; d = 370; e = 371 }]

    let inline_try () =
      let () =
        Alcotest.(
          check ~here:[%here]
            (list @@ list string)
            "mandatory fields when inlinable"
            [ [ "maybe"; "b"; "c" ] ]
          @@ descriptor_fields (inline_maybe_schema inlined_schema).descriptor)
      and () =
        test ~here:[%here] "inlinable" (inline_maybe inlined)
          (inline_maybe_schema inlined_schema)
          { maybe = true; inline = { b = 411; c = 412 } }
          [%ezjsonm { maybe = true; b = 411; c = 412 }]
      and () =
        let schema = inline_maybe_schema int_schema in
        let () =
          Alcotest.(
            check ~here:[%here]
              (list @@ list string)
              "mandatory fields when not inlinable"
              [ [ "maybe"; "inline" ] ]
            @@ descriptor_fields schema.descriptor)
        and () =
          test ~here:[%here] "not inlinable"
            (inline_maybe Alcotest.int)
            schema
            { maybe = true; inline = 416 }
            [%ezjsonm { maybe = true; inline = 416 }]
        in
        ()
      in
      ()

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
      inlined : Schematic.Schemas.ignore inline_parametric; [@schematic.inline]
          (* inlined : string option inline_parametric; [@schematic.inline] *)
    }
    [@@deriving eq, schema, show]

    let parametric = Alcotest.testable pp_parametric equal_parametric

    let inline_parametric () =
      test ~here:[%here] "parametric inlinable" parametric parametric_schema
        { inlined = A { value = 457; param = () } }
        [%ezjsonm { value = 457 }]

    type 'a hide = 'a [@@deriving eq, schema, show]

    type inline_nested_optional = {
      inline_nested : inline_nested_opt; [@schematic.inline]
    }

    and inline_nested_opt = { inlined : inlined option [@schematic.inline] }
    [@@deriving eq, schema, show]

    type inline_nested_default = {
      inline_nested : inline_nested_def; [@schematic.inline]
    }

    and inline_nested_def = {
      inlined : inlined option hide;
          [@schematic.default None] [@schematic.inline]
    }
    [@@deriving eq, schema, show]

    let inline_nested_optional () =
      let inline_nested_optional =
        Alcotest.testable pp_inline_nested_optional equal_inline_nested_optional
      in
      test ~here:[%here] "optional inline filled" inline_nested_optional
        inline_nested_optional_schema
        { inline_nested = { inlined = Some { b = 665; c = 666 } } }
        [%ezjsonm { b = 665; c = 666 }];
      test ~here:[%here] "optional inline empty" inline_nested_optional
        inline_nested_optional_schema
        { inline_nested = { inlined = None } }
        (`O [])

    let inline_nested_default () =
      let inline_nested_default =
        Alcotest.testable pp_inline_nested_default equal_inline_nested_default
      in
      test ~here:[%here] "default inline filled" inline_nested_default
        inline_nested_default_schema
        { inline_nested = { inlined = Some { b = 665; c = 666 } } }
        [%ezjsonm { b = 665; c = 666 }];
      test ~here:[%here] "default inline empty" inline_nested_default
        inline_nested_default_schema
        { inline_nested = { inlined = None } }
        (`O [])
  end

  (* Time *)

  type event = {
    start : Ptime.t;
    endd_ : Ptime.t;
  }

  and event_local = {
    start_local : Ptime.local;
    end_local : Ptime.local;
  }
  [@@deriving schema]

  let pp_event fmt e =
    let pp = Ptime.pp_rfc3339 () in
    Fmt.pf fmt "%a-%a" pp e.start pp e.endd_

  let pp_event_local fmt e =
    let pp fmt (t, tz) = Ptime.pp_rfc3339 ~tz_offset_s:tz () fmt t in
    Fmt.pf fmt "%a-%a" pp e.start_local pp e.end_local

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
    test ~here:[%here] "time" test_time event_schema
      {
        start = make_time "2020-05-08T23:48:34+02:00";
        endd_ = make_time "2020-05-08T23:48:47+02:00";
      }
      [%ezjsonm
        {
          start = "2020-05-08T21:48:34-00:00";
          endd = "2020-05-08T21:48:47-00:00";
        }];
    if Coding.local_time_support then
      test ~here:[%here] "time-local" test_time_local event_local_schema
        {
          start_local = make_time_local "2020-05-08T23:48:34+02:00";
          end_local = make_time_local "2020-05-08T23:48:47+02:00";
        }
        [%ezjsonm
          {
            start_local = "2020-05-08T23:48:34+02:00";
            end_local = "2020-05-08T23:48:47+02:00";
          }]

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
    test ~here:[%here] "missing optional inline" t
      optional_inline_wrapper_schema { inline = None; c = 377 }
      [%ezjsonm { c = 377 }];
    test ~here:[%here] "filled optional inline" t optional_inline_wrapper_schema
      {
        inline =
          Some { a = 375; b = 376; inline2 = { d = 378 }; missing = None };
        c = 377;
      }
      [%ezjsonm { a = 375; b = 376; d = 378; c = 377 }]

  type keyed =
    | Paragraph_ of { content : string } [@schematic.as "paragraph"]
    | Header of {
        content : string;
        level : int;
      }
    | Separator
  [@@schematic.name "keyed"] (* Check multiple attributes work *)
  [@@schematic.union_key "typ"]
  [@@deriving eq, schema, show]

  let keyed () =
    let tester = Alcotest.(testable pp_keyed equal_keyed) in
    let () =
      test ~here:[%here] "paragraph" tester keyed_schema
        (Paragraph_ { content = "text" })
        [%ezjsonm { content = "text"; typ = "paragraph" }]
    and () =
      test ~here:[%here] "header" tester keyed_schema
        (Header { content = "text"; level = 411 })
        [%ezjsonm { content = "text"; level = 411; typ = "header" }]
    and () =
      test ~here:[%here] "header" tester keyed_schema Separator
        [%ezjsonm { typ = "separator" }]
    and () =
      let json = [%ezjsonm { content = "text"; level = 411 }] in
      Alcotest.(
        check ~here:[%here]
          (result tester decoding_error)
          "values are rejected if the key is missing"
          (make_error ~schema:"keyed" {|missing key field "typ"|})
          (Schematic.Json.decode keyed_schema json))
    and () =
      let json = [%ezjsonm { content = "text"; level = 411; typ = "fumble" }] in
      Alcotest.(
        check ~here:[%here]
          (result tester decoding_error)
          "values are rejected if the key not recognized"
          (make_error ~schema:"keyed" {|invalid key field "typ" = "fumble"|})
          (Schematic.Json.decode keyed_schema json))
    and () =
      let json = [%ezjsonm { typ = "header"; content = "text" }] in
      Alcotest.(
        check ~here:[%here]
          (result tester decoding_error)
          "errors are specific to the matched case schema"
          (make_error ~schema:"keyed" {|missing required field "level"|})
          (Schematic.Json.decode keyed_schema json))
    in
    ()

  let map_error () =
    let schema =
      Schematic.Schema.make
        (Map
           {
             decode = (fun s -> Result.fail s);
             encode = Fn.id;
             descriptor = String;
           })
    in
    Alcotest.(
      check ~here:[%here]
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
      Alcotest.check ~here:[%here] json "json-schema" expected effective

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

    let pp_default fmt { a; b } = Fmt.pf fmt "(%i, %s)" a b

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
      test ~here:[%here] "default-int" tester default_schema
        { a = 358; b = "---" } [%ezjsonm { b = "---" }];
      test ~here:[%here] "default-string" tester default_schema
        { a = 359; b = "359" } [%ezjsonm { a = 359 }];
      test ~here:[%here] "default-both" tester default_schema
        { a = 358; b = "359" } (`O []);
      test ~here:[%here] "default-none" tester default_schema
        { a = 359; b = "---" } [%ezjsonm { a = 359; b = "---" }]
  end

  module Comparison = struct
    open Alcotest

    let check ~here name schema e l r =
      check ~here int name e (Schematic.Schema.compare schema l r)

    let bool () =
      let check = check "bool" bool_schema in
      let () = check ~here:[%here] 0 true true in
      let () = check ~here:[%here] 0 false false in
      let () = check ~here:[%here] 1 true false in
      let () = check ~here:[%here] (-1) false true in
      ()

    let int () =
      let check = check "int" int_schema in
      let () = check ~here:[%here] 0 0 0 in
      let () = check ~here:[%here] (-1) 0 1 in
      let () = check ~here:[%here] 1 1 0 in
      ()

    let string () =
      let check = check "string" string_schema in
      let () = check ~here:[%here] 0 "a" "a" in
      let () = check ~here:[%here] (-1) "a" "b" in
      let () = check ~here:[%here] 1 "b" "a" in
      let () = check ~here:[%here] 1 "aa" "a" in
      let () = check ~here:[%here] (-1) "a" "aa" in
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
      Base.List.iter ~f:(fun (exp, l, r) -> check exp (f l) (f r)) tests

    let record () =
      let check = check "version" version_schema in
      versions (check ~here:[%here]) version

    let tuple () =
      let check = check "version" version_tuple_schema in
      versions (check ~here:[%here]) Fn.id

    let variant () =
      let check = check "variant" expectation_schema
      and v = { major = 1; minor = 2; subminor = 3 } in
      let () = check ~here:[%here] 0 (Exact v) (Exact v)
      and () = check ~here:[%here] (-1) (Exact v) Nothing
      and () = check ~here:[%here] 1 Nothing (Exact v) in
      ()
  end

  module Error_messages = struct
    let test ~here name schema str e =
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
        check ~here (result unit string) name (Result.Error e)
          (Result.map_error ~f:Schematic.Error.to_string res))

    let missing () =
      let () =
        test ~here:[%here] "missing fields are reported" Recursive.schema
          {|{"value":0,"next":{"value":1,"next":{}}}|}
          {|path next.next: schema recursive: missing required field "value"|}
      and () =
        test ~here:[%here] "root error display no path" Recursive.schema {|{}|}
          {|schema recursive: missing required field "value"|}
      in
      ()

    let type_error () =
      test ~here:[%here] "missing" Recursive.schema
        {|{"value":0,"next":{"value":1,"next":{"value":"nope"}}}|}
        {|path next.next.value: expected integer, got string|}
  end

  (* Phantom types *)

  type _ id = string [@@deriving schema]
  type some_id = int id [@@deriving schema]

  type identified = {
    id : identified id; [@schematic.schema fun v -> id_schema_versioned v ()]
  }
  [@@deriving schema]

  module Names = struct
    type foo = int [@@deriving schema]
    type t = int [@@deriving schema]

    let deduced () =
      Alcotest.(
        check ~here:[%here] (option string)
          "schema name is deduced from type name" (Some "foo") foo_schema.id);
      Alcotest.(
        check ~here:[%here] (option string)
          "schema name is deduced from type module name when type is \"t\""
          (Some "names") schema.id);
      Alcotest.(
        check ~here:[%here] (option string)
          "schema name is deduced from file name when type is \"t\""
          (Some "tests") root_schema.id)

    type 'a result =
      | Ok of 'a
      | Error
    [@@deriving schema]

    let parametric () =
      Alcotest.(
        check ~here:[%here] (option string)
          "schema name is deduced from type name" (Some "foo-result")
          (result_schema foo_schema).id);
      Alcotest.(
        check ~here:[%here] (option string)
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
    Alcotest.testable
      (pp_collision_2 Fmt.string)
      (equal_collision_2 String.equal)

  let collision () =
    let () =
      test ~here:[%here] "collision_1" collision_1 collision_1_schema
        ({ collision_a = 725; collision_b = "725" } : collision_1)
        [%ezjsonm { collision_a = 725; collision_b = "725" }]
    and () =
      test ~here:[%here] "collision_2" collision_2
        (collision_2_schema Schematic.Schemas.string_schema)
        ({ collision_a = 725; collision_b = "725" } : string collision_2)
        [%ezjsonm { collision_a = 725; collision_b = "725" }]
    in
    ()

  (* Computed *)

  module Computed = struct
    type t = {
      x : int;
      y : int; [@default 0]
    }
    [@@schematic.computed
      {
        sum : int = x + y;
        opposite : int option = Option.some_if (x <> 0) (-x);
      }]
    [@@deriving eq, schema, show]

    let t = Alcotest.testable pp equal

    let computed () =
      test ~here:[%here] "computed" t schema { x = 1; y = 2 }
        [%ezjsonm { x = 1; y = 2; sum = 3; opposite = -1 }];
      test ~here:[%here] "computed optional" t schema { x = 0; y = 0 }
        [%ezjsonm { x = 0; sum = 0 }];
      (* This used to fail because the computed field was expected *)
      Alcotest.(check ~here:[%here] (result t decoding_error))
        "variant containing records with missing computed field"
        (Result.Ok { x = 1198; y = 33 })
      @@ Schematic.Json.decode schema [%ezjsonm { x = 1198; y = 33 }]
  end

  (* Version *)

  module Version = struct
    type v =
      | Int of int
      | String of string
    [@@deriving eq, schema, show]

    let legacy _ =
      let decode = function
        | None -> Result.return None
        | Some i -> Result.return (Some (Int i))
      and encode = function Some (Int i) -> Some i | _ -> None in
      Schematic.Schema.make
      @@ Map
           {
             decode;
             encode;
             descriptor = Nullable Schematic.Schemas.int_schema;
           }

    type t = {
      v : v Option.t;
          [@schematic.schema legacy < "0.6.1"] [@schematic.as "w" < "0.6.1"]
    }
    [@@deriving eq, schema, show]

    let t = Alcotest.testable pp equal

    let schema_versions = function
      | Schemas_library.V -> Some (Schematic.Version.make 0 6 0)
      | _ -> None

    let test () =
      test ~here:[%here] "latest none" t schema { v = None }
        [%ezjsonm { v = None }];
      test ~here:[%here] "latest int" t schema { v = Some (Int 3) }
        [%ezjsonm { v = 3 }];
      test ~here:[%here] "latest string" t schema { v = Some (String "3") }
        [%ezjsonm { v = "3" }];
      test ~here:[%here] "old none" t
        (schema_versioned schema_versions)
        { v = None } [%ezjsonm { w = None }];
      test ~here:[%here] "old int" t
        (schema_versioned schema_versions)
        { v = Some (Int 3) } [%ezjsonm { w = 3 }];
      test ~here:[%here] "old string" t
        (schema_versioned schema_versions)
        { v = Some (String "3") } ~decoded:{ v = None } [%ezjsonm { w = None }]
  end

  module VersionCascade = struct
    type inner = {
      content : string;
          [@schematic.as "content_old" < "1.0.0"] [@schematic.as "content_new"]
    }
    [@@deriving eq, schema, show]

    type outer = {
      id : string;
      content : inner;
    }
    [@@deriving eq, schema, show]

    let legacy version : outer Schema.t =
      let inner_schema = inner_schema_versioned version in
      let decode (inner : inner) : (outer, string) Result.t =
        Result.return { id = "glouglou"; content = inner }
      and encode ({ content = { content }; _ } : outer) : inner = { content } in
      let descriptor =
        Schematic.Schema.Map
          { decode; encode; descriptor = inner_schema.descriptor }
      in
      Schematic.Schema.make descriptor

    type t = {
      t_ : outer;
          [@schematic.schema legacy < "1.0.0"]
          [@schematic.schema outer_schema_versioned]
          [@schematic.inline]
    }
    [@@deriving eq, schema, show]

    let testable = Alcotest.testable pp equal

    let test () =
      let () =
        let value = { id = "glouglou"; content = { content = "foo" } } in
        let value = { t_ = value } in
        let json =
          [%ezjsonm { id = "glouglou"; content = { content_new = "foo" } }]
        in
        test ~here:[%here] "latest" testable schema value json
      in
      let () =
        let schema_versions = function
          | Schemas_library.V -> Some (Schematic.Version.make 0 9 0)
          | _ -> None
        in
        let value = { id = "glouglou"; content = { content = "foo" } } in
        let value = { t_ = value } in
        let json = [%ezjsonm { content_old = "foo" }] in
        let schema = schema_versioned schema_versions in
        test ~here:[%here] "legacy" testable schema value json
      in
      ()
  end

  module Compile_check = struct
    (* Name conflict between parameter and type *)
    type 't t = { items : 't list } [@@deriving schema]
  end

  module Pp = struct
    module T = struct
      type t =
        | List of t list
        | Nullable of t option
        | Object of { value : t }
        | Tuple of (t * t)
      [@@schematic.name "graph-result"]
      [@@deriving eq, ord, schema, show { with_path = false }]
    end

    let text = Alcotest.testable (Fmt.fmt "{|%s|}") String.equal

    let pp_schema () =
      Alcotest.(check ~here:[%here] text)
        "schema pretty print"
        {|<graph-result> = Union [
                   { name: "list", schema: [ <graph-result> ] },
                   { name: "nullable", schema: Nullable (<graph-result>) },
                   {
                   name: "object",
                   schema: { value (Required): <graph-result>,  }
                   },
                   {
                   name: "tuple",
                   schema: [ <graph-result>, <graph-result>,  ]
                   }
                 ]|}
      @@ Fmt.str "%a" Schematic.Schema.pp
      @@ T.schema_versioned (fun _ -> None)

    let pp_version = Schematic.Pp.of_schema version_schema
    let pp_range = Schematic.Pp.of_schema range_schema

    let pp () =
      let version = { major = 1242; minor = 1243; subminor = 1244 } in
      Alcotest.(
        check ~here:[%here] string "version"
          {|{major: 1242; minor: 1243; subminor: 1244;}|}
          (Fmt.str "@[%a@]" pp_version version));
      Alcotest.(
        check ~here:[%here] string "range"
          {|(
  {major: 1242; minor: 1243; subminor: 1244;},
  {major: 1242; minor: 1243; subminor: 1244;},
  1245
)|}
          (Fmt.str "@[%a@]" pp_range (version, version, 1245)));
      let buffer = Buffer.create 1024 in
      let fmt = Stdlib.Format.formatter_of_buffer buffer in
      let () = Stdlib.Format.pp_set_margin fmt 20 in
      let () = Fmt.pf fmt "@[%a@]" pp_version version in
      let () = Stdlib.Format.pp_print_flush fmt () in
      Alcotest.(
        check ~here:[%here] string "version-narrow"
          {|{
  major: 1242;
  minor: 1243;
  subminor: 1244;
}|}
          (Buffer.contents buffer))
  end

  module Extension = struct
    let atomic () =
      test ~here:[%here] "integer" Alcotest.int [%schema: int] 42 [%ezjsonm 42]

    let composite () =
      test ~here:[%here] "pair"
        Alcotest.(pair int string)
        [%schema: int * string] (42, "forty two") [%ezjsonm [ 42; "forty two" ]]
  end

  (* Driver *)

  let test_descriptions : test_group list =
    [
      {
        name = "coding2";
        test_cases =
          [
            atom_int;
            atom_string;
            Optionals.options;
            Optionals.nullable_option;
            Optionals.default_option;
            Optionals.disabled_optionals;
            record;
            tuple;
            Variant.variant;
            Variant.singleton;
            Variant.computed;
            Variant.no_match;
            Variant.null_object;
            Polymorphic_variant.payload;
            Polymorphic_variant.keyed;
          ];
      };
    ]

  let tests2 : (string * unit Alcotest.test_case list) list =
    let test_case_to_alcotest_test_case { name; tests; speed; additional_test }
        =
      let alcotest_test () =
        let () = Base.List.iter ~f:test2 tests in
        match additional_test with
        | None -> ()
        | Some additional_test -> additional_test ()
      in
      Alcotest.test_case name speed alcotest_test
    in
    let test_group_to_alcotest_group { name; test_cases } =
      (name, Base.List.map ~f:test_case_to_alcotest_test_case test_cases)
    in
    Base.List.map ~f:test_group_to_alcotest_group test_descriptions

  (* TODO Migrate everything to [test_descriptions], remove this and rename
     [tests2]. *)
  let tests : (string * unit Alcotest.test_case list) list =
    let open Alcotest in
    [
      ( "coding",
        [
          test_case "alias" `Quick alias;
          test_case "list" `Quick List.objects;
          test_case "option list" `Quick List.option;
          test_case "parametric_type" `Quick Parametric.simple;
          test_case "anonymous_parameter" `Quick Parametric.anonymous;
          test_case "simple GADT" `Quick Gadt.simple;
          test_case "complex GADT" `Quick Gadt.complex;
          test_case "escape" `Quick escape;
          test_case "recursive" `Quick Recursive.plain;
          test_case "recursive_parametric" `Quick Recursive.parametric;
          test_case "mutually recursive" `Quick mutually;
          test_case "ignored" `Quick ignored;
          test_case "inline-record" `Quick Inline.record;
          test_case "inline-variant-payload" `Quick Inline.variant_payload;
          test_case "inline-variant" `Quick Inline.inline_variant;
          test_case "inline-try" `Quick Inline.inline_try;
          test_case "inline-parametric" `Quick Inline.inline_parametric;
          test_case "inline-nested-optional" `Quick
            Inline.inline_nested_optional;
          test_case "inline-nested-default" `Quick Inline.inline_nested_default;
          test_case "time" `Quick time;
          test_case "json-schema-metadata-description" `Quick
            Metadata.description;
          test_case "json-schema-metadata-default" `Quick Metadata.default;
          test_case "failure" `Quick failure;
          test_case "optional-inline" `Quick optional_inline;
          test_case "keyed" `Quick keyed;
          test_case "map-error" `Quick map_error;
          test_case "collisions" `Quick collision;
          test_case "computed" `Quick Computed.computed;
          test_case "version" `Quick Version.test;
          test_case "version_cascade" `Quick VersionCascade.test;
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
      ( "pp",
        [
          test_case "pp" `Quick Pp.pp; test_case "pp schema" `Quick Pp.pp_schema;
        ] );
      ( "extension",
        [
          test_case "atomic" `Quick Extension.atomic;
          test_case "composite" `Quick Extension.composite;
        ] );
    ]
end

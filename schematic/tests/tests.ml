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
      is_kotlin_roundtrip_enabled : bool;
      swift_class_name : string option;
      is_swift_roundtrip_enabled : bool;
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
  val to_string : t -> string
  val streaming : (module Schematic.Streaming.S) option
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
           swift_class_name = _;
           is_swift_roundtrip_enabled = _;
         } as test_descripton) =
    let encoded = Coding.encode schema value in
    let () =
      Fmt.epr "Running test2 for schema %a@." Schematic.Schema.pp schema
    in
    let () = test ~here ?decoded name value_testable schema value expected in
    let () =
      match Coding.streaming with
      | None -> ()
      | Some (module S : Schematic.Streaming.S) ->
        let streaming = S.to_string schema value in
        Alcotest.(check ~here string)
          (Fmt.str "%s: streaming matches coding" name)
          (Coding.to_string @@ Coding.encode schema value)
          streaming
    in
    Coding.extra_test test_descripton encoded

  module Opt = struct
    include Option

    let schema_versioned = Schematic.Schemas.option_schema_versioned

    let pp pp =
      Fmt.(option ~none:(const string "None") (const string "Some " ++ pp))
  end

  (* Failure *)

  let failure =
    {
      name = "failure";
      speed = `Quick;
      additional_test =
        Some
          (fun () ->
            if Option.is_some (Coding.of_json [%ezjsonm 29]) then
              if
                try
                  test ~here:[%here] "int atom" Alcotest.int int_schema 28
                    [%ezjsonm 29];
                  true
                with _ -> false
              then Alcotest.fail "failure test failed");
      tests = [];
    }

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
              swift_class_name = Some "Swift.Int";
              is_swift_roundtrip_enabled = true;
            };
        ];
    }

  let atom_int64 =
    {
      name = "int64";
      speed = `Quick;
      additional_test = None;
      tests =
        [
          Test
            {
              name = "int64 atom";
              here = [%here];
              decoded = None;
              value_testable = Alcotest.int64;
              schema = int64_schema;
              value = 9223372036854L;
              expected = [%ezjsonm 9223372036854];
              kotlin_class_name = Some "java.lang.Long";
              is_kotlin_roundtrip_enabled = true;
              swift_class_name = Some "Swift.Int64";
              is_swift_roundtrip_enabled = true;
            };
        ];
    }

  let atom_float =
    {
      name = "float";
      speed = `Quick;
      additional_test = None;
      tests =
        [
          Test
            {
              name = "float atom";
              here = [%here];
              decoded = None;
              value_testable = Alcotest.float 0.00001;
              schema = float_schema;
              value = 1.234567890;
              expected = `Float 1.234567890;
              kotlin_class_name = Some "java.lang.Float";
              is_kotlin_roundtrip_enabled = true;
              swift_class_name = Some "Swift.Float";
              is_swift_roundtrip_enabled = true;
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
              swift_class_name = Some "Swift.String";
              is_swift_roundtrip_enabled = true;
            };
        ];
    }

  let atom_date =
    {
      name = "date";
      speed = `Quick;
      additional_test = None;
      tests =
        [
          Test
            {
              name = "date atom";
              here = [%here];
              decoded = None;
              value_testable = Alcotest.(triple int int int);
              schema = Ptime.date_schema;
              value = (2024, 8, 25);
              expected = [%ezjsonm [ 2024; 8; 25 ]];
              kotlin_class_name = Some "kotlinx.datetime.LocalDate";
              is_kotlin_roundtrip_enabled = true;
              swift_class_name = Some "TimelessDate";
              is_swift_roundtrip_enabled = true;
            };
        ];
    }

  (* Coding *)

  let coding =
    {
      name = "coding descriptors";
      speed = `Quick;
      tests =
        [
          Test
            {
              name = "json";
              here = [%here];
              decoded = None;
              schema =
                Schema.make
                @@ Coding
                     {
                       f =
                         (fun (type coding)
                           (coding : coding Schematic.Coding.coding option)
                         ->
                           match coding with
                           | Some Schematic.Json.Coding ->
                             Schema.Map
                               {
                                 encode = (fun i -> i + 1);
                                 decode = (fun i -> Result.Ok (i - 1));
                                 descriptor = Schema.Int;
                               }
                           | _ -> Int);
                     };
              kotlin_class_name = None;
              is_kotlin_roundtrip_enabled = false;
              swift_class_name = None;
              is_swift_roundtrip_enabled = false;
              value = 2992;
              expected = `Float 2993.0;
              value_testable = Alcotest.int;
            };
        ];
      additional_test = None;
    }

  (* Optionals *)

  module Optionals = struct
    let options =
      let mk_test name value expected =
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
            is_kotlin_roundtrip_enabled =
              false
              (* Kotlin's [Class.forName] does not support generics/nullable
                 types *);
            swift_class_name = Some "Routine.Maybe";
            is_swift_roundtrip_enabled = false;
          }
      in
      {
        name = "option";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            mk_test "some int" (Some 28) [%ezjsonm 28];
            mk_test "none int" None [%ezjsonm None];
          ];
      }

    (* Nullable option *)

    type nullable_option = { no : int option option }
    [@@deriving eq, schema, show]

    let nullable_option =
      Alcotest.testable pp_nullable_option equal_nullable_option

    let nullable_option =
      let mk_test name value expected =
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
            is_kotlin_roundtrip_enabled = true;
            swift_class_name = None;
            is_swift_roundtrip_enabled = true;
          }
      in
      {
        name = "nullable option";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            mk_test "omitted" { no = None } (`O []);
            mk_test "null" { no = Some None } [%ezjsonm { no = None }];
            mk_test "specified" { no = Some (Some 74) } [%ezjsonm { no = 74 }];
          ];
      }

    (* Default option*)

    type default_option = { oo : int Opt.t Opt.t [@default None] }
    [@@deriving eq, schema, show]

    let default_option =
      Alcotest.(testable pp_default_option equal_default_option)

    let default_option =
      let mk_test name value expected =
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
            is_kotlin_roundtrip_enabled = true;
            swift_class_name = None;
            is_swift_roundtrip_enabled = true;
          }
      in
      {
        name = "default option";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            mk_test "none" { oo = None } (`O []);
            mk_test "some none" { oo = Some None } [%ezjsonm { oo = None }];
            mk_test "some some" { oo = Some (Some 877) } [%ezjsonm { oo = 877 }];
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
                is_kotlin_roundtrip_enabled = true;
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
              };
          ];
      }
  end

  (* Record *)

  (** A version description *)
  type version = {
    major : int;  (** Major version number *)
    minor : int;
    subminor : int;
  }
  [@@deriving eq, schema]

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
              swift_class_name = None;
              is_swift_roundtrip_enabled = true;
            };
        ];
    }

  (* Alias *)

  type version_alias = version [@@deriving schema]

  let alias =
    {
      name = "alias";
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
              schema = version_alias_schema;
              value = v2;
              expected = [%ezjsonm { major = 3; minor = 4; subminor = 5 }];
              kotlin_class_name = None;
              is_kotlin_roundtrip_enabled = true;
              swift_class_name = None;
              is_swift_roundtrip_enabled = true;
            };
        ];
    }

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
              swift_class_name = None;
              is_swift_roundtrip_enabled = true;
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
            swift_class_name = None;
            is_swift_roundtrip_enabled = false;
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
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
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
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
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
                is_kotlin_roundtrip_enabled = true;
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
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
      let mk_test name value expected =
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
            is_kotlin_roundtrip_enabled = true;
            swift_class_name = None;
            is_swift_roundtrip_enabled = true;
          }
      in
      {
        name = "polymorphic variant";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            mk_test "polymorphic variant" `Mon [%ezjsonm "mon"];
            mk_test "polymorphic variant alias" `Tue [%ezjsonm "tuesday"];
            mk_test "polymorphic variant payload" (`Wed 160) [%ezjsonm 160];
            mk_test "polymorphic variant composite payload"
              (`Thu (true, false))
              [%ezjsonm [ true; false ]];
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
      let mk_test name value expected =
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
            is_kotlin_roundtrip_enabled = true;
            swift_class_name = None;
            is_swift_roundtrip_enabled = true;
          }
      in
      {
        name = "keyed polymorphic variant";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            mk_test "keyed polymorphic variant" `Left
              [%ezjsonm { position = "left" }];
            mk_test "keyed polymorphic variant"
              (`Center { stable = false })
              [%ezjsonm { position = "center"; stable = false }];
          ];
      }
  end

  (* List *)

  module List = struct
    let objects =
      let mk_test name value expected =
        let test schema =
          Test
            {
              name;
              here = [%here];
              decoded = None;
              value_testable = Alcotest.list test_version;
              schema;
              value;
              expected;
              kotlin_class_name = None;
              is_kotlin_roundtrip_enabled =
                false
                (* Cannot find by reflection the class name for a list of X *);
              swift_class_name = None;
              is_swift_roundtrip_enabled = false;
            }
        in
        [
          test @@ list_schema version_schema;
          test
          @@ Schematic.Schema.make
          @@ Sequence
               {
                 construct = Fn.compose Result.return Base.Sequence.to_list;
                 deconstruct = Base.Sequence.of_list;
                 item = version_schema;
                 length = List.length;
               };
        ]
      in
      {
        name = "list";
        speed = `Quick;
        additional_test = None;
        tests =
          List.concat
            [
              mk_test "list" [] [%ezjsonm []];
              mk_test "list"
                [ { major = 0; minor = 1; subminor = 2 } ]
                [%ezjsonm [ { major = 0; minor = 1; subminor = 2 } ]];
              mk_test "list"
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
                  ]];
            ];
      }

    let option =
      let mk_test name value expected =
        Test
          {
            name;
            here = [%here];
            decoded = None;
            value_testable = Alcotest.(list (option int));
            schema = list_schema (option_schema int_schema);
            value;
            expected;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled =
              false
              (* Cannot find by reflection the class name for a list of X *);
            swift_class_name = None;
            is_swift_roundtrip_enabled = false;
          }
      in
      {
        name = "option list";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            mk_test "empty" [] [%ezjsonm []];
            mk_test "some" [ Some 1; Some 2; Some 3 ] [%ezjsonm [ 1; 2; 3 ]];
            mk_test "none" [ None; None; None ] [%ezjsonm [ None; None; None ]];
            mk_test "mixed" [ Some 1; None; Some 3 ] [%ezjsonm [ 1; None; 3 ]];
          ];
      }
  end

  module Mapping = struct
    module T = struct
      type t = (string * version) list [@@deriving eq, schema, show]
    end

    let objects =
      let mk_test name value expected =
        Test
          {
            name;
            here = [%here];
            decoded = None;
            value_testable = (module T);
            schema =
              Schema.make
              @@ Mapping
                   {
                     construct = Fn.compose Result.return Base.Sequence.to_list;
                     deconstruct = Base.Sequence.of_list;
                     item = version_schema;
                     length = Stdlib.List.length;
                   };
            value;
            expected;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled =
              false
              (* Cannot find by reflection the class name for a list of X *);
            swift_class_name = None;
            is_swift_roundtrip_enabled = false;
          }
      in
      {
        name = "list";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            mk_test "empty" [] (`O []);
            mk_test "filled"
              [
                ("one", { major = 0; minor = 1; subminor = 2 });
                ("two", { major = 3; minor = 4; subminor = 5 });
              ]
              [%ezjsonm
                {
                  one = { major = 0; minor = 1; subminor = 2 };
                  two = { major = 3; minor = 4; subminor = 5 };
                }];
          ];
      }

    let option =
      let mk_test name value expected =
        Test
          {
            name;
            here = [%here];
            decoded = None;
            value_testable = Alcotest.(list (option int));
            schema = list_schema (option_schema int_schema);
            value;
            expected;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled =
              false
              (* Cannot find by reflection the class name for a list of X *);
            swift_class_name = None;
            is_swift_roundtrip_enabled = false;
          }
      in
      {
        name = "option list";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            mk_test "list" [] [%ezjsonm []];
            mk_test "list" [ Some 1; Some 2; Some 3 ] [%ezjsonm [ 1; 2; 3 ]];
            mk_test "list" [ None; None; None ] [%ezjsonm [ None; None; None ]];
            mk_test "list" [ Some 1; None; Some 3 ] [%ezjsonm [ 1; None; 3 ]];
          ];
      }
  end

  (* Parametric *)

  module Parametric = struct
    type 'a pair = 'a * 'a [@@deriving schema]

    let simple =
      {
        name = "parametric_type";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            Test
              {
                name = "parametric_type";
                here = [%here];
                decoded = None;
                value_testable = Alcotest.pair test_version test_version;
                schema = pair_schema version_schema;
                value = (v1, v3);
                expected =
                  [%ezjsonm
                    [
                      { major = 0; minor = 1; subminor = 2 };
                      { major = 6; minor = 7; subminor = 8 };
                    ]];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled =
                  false
                  (* Cannot find by reflection the class name for a
                     [typealias] *);
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
              };
          ];
      }

    type _ anonymous = int [@@deriving schema]

    let anonymous =
      {
        name = "anonymous_parameter";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            Test
              {
                name = "anonymous parameter";
                here = [%here];
                decoded = None;
                value_testable = Alcotest.int;
                schema = anonymous_schema version_schema;
                value = 345;
                expected = [%ezjsonm 345];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled =
                  false
                  (* Cannot find by reflection the class name for a
                     [typealias] *);
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
              };
          ];
      }
  end

  (* GADT *)

  module Gadt = struct
    type _ simple = Int : int simple [@@deriving eq, schema, show]

    let simple = Alcotest.testable pp_simple equal_simple

    let simple =
      {
        name = "simple GADT";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            Test
              {
                name = "simple GADT";
                here = [%here];
                decoded = None;
                value_testable = simple;
                schema = simple_schema version_schema;
                value = Int;
                expected = [%ezjsonm "int"];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled = true;
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
              };
          ];
      }

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

    let complex =
      {
        name = "complex GADT";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            Test
              {
                name = "complex GADT";
                here = [%here];
                decoded = None;
                value_testable = Alcotest.testable pp_complex equal_complex;
                schema = int_complex_schema int_schema;
                value = Int;
                expected = [%ezjsonm "int"];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled = true;
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
              };
            Test
              {
                name = "complex GADT";
                here = [%here];
                decoded = None;
                value_testable = Alcotest.testable pp_complex equal_complex;
                schema = string_complex_schema string_schema;
                value = String "386";
                expected = [%ezjsonm "386"];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled = true;
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
              };
          ];
      }
  end

  (* Escape *)

  type escape_record = {
    special_char : int; [@schematic.as "'char$"]
    keyword : string; [@schematic.as "if"]
  }
  [@@schematic.name "if"]

  and escape = If of escape_record [@@deriving schema]

  let pp_escape fmt (If { special_char; keyword }) =
    Fmt.pf fmt "(%i, %s)" special_char keyword

  let test_escape = Alcotest.testable pp_escape ( = )

  let escape =
    {
      name = "escape";
      speed = `Quick;
      additional_test = None;
      tests =
        [
          Test
            {
              name = "escape";
              here = [%here];
              decoded = None;
              value_testable = test_escape;
              schema = escape_schema;
              value = If { special_char = 128; keyword = "coin" };
              expected = `O [ ("'char$", `Float 128.); ("if", `String "coin") ];
              kotlin_class_name = None;
              is_kotlin_roundtrip_enabled = true;
              swift_class_name = None;
              is_swift_roundtrip_enabled = true;
            };
        ];
    }

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

    let plain =
      {
        name = "recursive";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            Test
              {
                name = "recursive";
                here = [%here];
                decoded = None;
                value_testable = test_recursive;
                schema;
                value = { value = 0; next = Some { value = 1; next = None } };
                expected = [%ezjsonm { value = 0; next = { value = 1 } }];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled = true;
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
              };
          ];
      }

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

    let parametric =
      {
        name = "recursive_parametric";
        speed = `Quick;
        tests = [];
        additional_test =
          Some
            (fun () ->
              let () =
                Alcotest.(check ~here:[%here] int "first count" 0 !count)
              in
              let schema = parametric_schema int_schema in
              let () =
                Alcotest.(check ~here:[%here] int "second count" 1 !count)
              in
              let _schema = parametric_schema int_schema in
              let () =
                Alcotest.(check ~here:[%here] int "third count" 1 !count)
              in
              let () =
                test ~here:[%here] "recursive" (parametric Alcotest.int) schema
                  {
                    count = ();
                    value = 0;
                    next = Some { count = (); value = 1; next = None };
                  }
                  [%ezjsonm { value = 0; next = { value = 1 } }]
              in
              Alcotest.(check ~here:[%here] int "fourth count" 1 !count));
      }
  end

  type mutually_a = { next_b : mutually_b option }
  and mutually_b = { next_a : mutually_a option } [@@deriving schema]

  let rec pp_mutually_a fmt { next_b } = Fmt.option pp_mutally_b fmt next_b
  and pp_mutally_b fmt { next_a } = Fmt.option pp_mutually_a fmt next_a

  let test_mutually_a = Alcotest.testable pp_mutually_a ( = )

  let mutually =
    {
      name = "mutually recursive";
      speed = `Quick;
      additional_test = None;
      tests =
        [
          Test
            {
              name = "mutually recursive";
              here = [%here];
              decoded = None;
              value_testable = test_mutually_a;
              schema = mutually_a_schema;
              value = { next_b = Some { next_a = Some { next_b = None } } };
              expected = [%ezjsonm { next_b = { next_a = [%aq `O []] } }];
              kotlin_class_name = None;
              is_kotlin_roundtrip_enabled = true;
              swift_class_name = None;
              is_swift_roundtrip_enabled = true;
            };
        ];
    }

  (* Ignored *)

  type ignored = {
    ignored : ignore;
    not_ignored : unit;
  }
  [@@deriving eq, schema, show]

  let ignored =
    let ignored = Alcotest.(testable pp_ignored equal_ignored) in

    {
      name = "ignored";
      speed = `Quick;
      additional_test = None;
      tests =
        [
          Test
            {
              name = "missing ignored fields are accepted";
              here = [%here];
              decoded = None;
              value_testable = ignored;
              schema = ignored_schema;
              value = { ignored = (); not_ignored = () };
              expected = [%ezjsonm { not_ignored = None }];
              kotlin_class_name = None;
              is_kotlin_roundtrip_enabled = true;
              swift_class_name = None;
              is_swift_roundtrip_enabled = true;
            };
        ];
    }

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

    and nested_inline_event =
      | Mention of { origin : string }
      | SetRelation of { relation : string }

    and nested_inline_inner_required = {
      created : string;
      message : string;
      event : nested_inline_event; [@schematic.inline]
          (* Required, not optional *)
    }

    and nested_inline_inner = {
      created : string;
      message : string;
      event : nested_inline_event option; [@schematic.inline]
    }

    and nested_inline_outer_required = {
      data : nested_inline_inner_required; [@schematic.inline]
      workspace_id : string;
    }

    and nested_inline_outer = {
      data : nested_inline_inner; [@schematic.inline]
      workspace_id : string;
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

    let nested_inline_inner_required =
      Alcotest.testable pp_nested_inline_inner_required
        equal_nested_inline_inner_required

    let nested_inline_outer_required =
      Alcotest.testable pp_nested_inline_outer_required
        equal_nested_inline_outer_required

    let nested_inline_inner =
      Alcotest.testable pp_nested_inline_inner equal_nested_inline_inner

    let nested_inline_outer =
      Alcotest.testable pp_nested_inline_outer equal_nested_inline_outer

    let descriptor_fields descriptor =
      match
        Schematic.Schema.(
          Inline.schema descriptor
            { f = (fun { name; _ } -> name); filter = None })
      with
      | Result.Ok sets -> sets
      | Result.Error e -> Alcotest.fail e

    let record =
      {
        name = "inline-record";
        speed = `Quick;
        tests =
          [
            Test
              {
                name = "coding";
                here = [%here];
                decoded = None;
                value_testable = inline;
                schema = inline_schema;
                value = { a = 0; coin = { b = 1; c = 2 }; d = 3; ignored = () };
                expected = [%ezjsonm { a = 0; b = 1; c = 2; d = 3 }];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled = true;
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
              };
          ];
        additional_test =
          Some
            (fun () ->
              Alcotest.(
                check ~here:[%here]
                  (list @@ list string)
                  "the list of mandatory fields"
                  [ [ "a"; "b"; "c"; "d" ] ]
                @@ descriptor_fields inline_schema.descriptor));
      }

    let variant_payload =
      {
        name = "inline-variant-payload";
        speed = `Quick;
        tests =
          [
            Test
              {
                name = "coding";
                here = [%here];
                decoded = None;
                value_testable = variant_inline;
                schema = variant_inline_schema;
                value =
                  Ctor { a = 0; coin = { b = 1; c = 2 }; d = 3; ignored = () };
                expected = [%ezjsonm { a = 0; b = 1; c = 2; d = 3 }];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled = true;
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
              };
          ];
        additional_test =
          Some
            (fun () ->
              Alcotest.(
                check ~here:[%here]
                  (list @@ list string)
                  "the list of mandatory fields"
                  [ [ "a"; "b"; "c"; "d" ]; [ "dummy" ] ]
                @@ descriptor_fields variant_inline_schema.descriptor));
      }

    let inline_variant =
      let mk_test name value expected =
        Test
          {
            name;
            here = [%here];
            decoded = None;
            value_testable = inline_variant;
            schema = inline_variant_schema;
            value;
            expected;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled = true;
            swift_class_name = None;
            is_swift_roundtrip_enabled = true;
          }
      in
      {
        name = "inline-variant";
        speed = `Quick;
        additional_test =
          Some
            (fun () ->
              Alcotest.(
                check ~here:[%here]
                  (list @@ list string)
                  "the list of mandatory fields"
                  [ [ "x"; "b"; "c" ]; [ "x"; "d"; "e" ] ]
                @@ descriptor_fields inline_variant_schema.descriptor));
        tests =
          [
            mk_test "coding of Foo"
              { x = 362; pan = Foo { b = 363; c = 364 } }
              [%ezjsonm { x = 362; b = 363; c = 364 }];
            mk_test "coding of Bar"
              { x = 369; pan = Bar { d = 370; e = 371 } }
              [%ezjsonm { x = 369; d = 370; e = 371 }];
          ];
      }

    let inline_try =
      let schema = inline_maybe_schema int_schema in

      {
        name = "inline-try";
        speed = `Quick;
        additional_test =
          Some
            (fun () ->
              let () =
                Alcotest.(
                  check ~here:[%here]
                    (list @@ list string)
                    "mandatory fields when inlinable"
                    [ [ "maybe"; "b"; "c" ] ]
                  @@ descriptor_fields
                       (inline_maybe_schema inlined_schema).descriptor)
              in
              Alcotest.(
                check ~here:[%here]
                  (list @@ list string)
                  "mandatory fields when not inlinable"
                  [ [ "maybe"; "inline" ] ]
                @@ descriptor_fields schema.descriptor));
        tests =
          [
            Test
              {
                name = "inlinable";
                here = [%here];
                decoded = None;
                value_testable = inline_maybe inlined;
                schema = inline_maybe_schema inlined_schema;
                value = { maybe = true; inline = { b = 411; c = 412 } };
                expected = [%ezjsonm { maybe = true; b = 411; c = 412 }];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled = true;
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
              };
            Test
              {
                name = "not inlinable";
                here = [%here];
                decoded = None;
                value_testable = inline_maybe Alcotest.int;
                schema;
                value = { maybe = true; inline = 416 };
                expected = [%ezjsonm { maybe = true; inline = 416 }];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled = true;
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
              };
          ];
      }

    let inline_nested_union_required =
      {
        name = "inline-nested-union-required";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            Test
              {
                name = "nested union required";
                here = [%here];
                decoded = None;
                value_testable = nested_inline_outer_required;
                schema = nested_inline_outer_required_schema;
                value =
                  {
                    data =
                      {
                        created = "2026-01-23T12:00:00Z";
                        message = "test";
                        event = Mention { origin = "channel-123" };
                      };
                    workspace_id = "workspace-123";
                  };
                expected =
                  [%ezjsonm
                    {
                      created = "2026-01-23T12:00:00Z";
                      message = "test";
                      origin = "channel-123";
                      workspace_id = "workspace-123";
                    }];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled = true;
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
              };
          ];
      }

    let inline_nested_union_optional =
      {
        name = "inline-nested-union-optional";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            Test
              {
                name = "nested union none";
                here = [%here];
                decoded = None;
                value_testable = nested_inline_outer;
                schema = nested_inline_outer_schema;
                value =
                  {
                    data =
                      {
                        created = "2026-01-23T12:00:00Z";
                        message = "test";
                        event = None;
                      };
                    workspace_id = "workspace-123";
                  };
                expected =
                  [%ezjsonm
                    {
                      created = "2026-01-23T12:00:00Z";
                      message = "test";
                      workspace_id = "workspace-123";
                    }];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled = true;
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
              };
            Test
              {
                name = "nested union some";
                here = [%here];
                decoded = None;
                value_testable = nested_inline_outer;
                schema = nested_inline_outer_schema;
                value =
                  {
                    data =
                      {
                        created = "2026-01-23T12:00:00Z";
                        message = "test";
                        event = Some (Mention { origin = "channel-123" });
                      };
                    workspace_id = "workspace-123";
                  };
                expected =
                  [%ezjsonm
                    {
                      created = "2026-01-23T12:00:00Z";
                      message = "test";
                      origin = "channel-123";
                      workspace_id = "workspace-123";
                    }];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled = true;
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
              };
          ];
      }

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

    let inline_parametric =
      {
        name = "inline-parametric";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            Test
              {
                name = "parametric inlinable";
                here = [%here];
                decoded = None;
                value_testable = parametric;
                schema = parametric_schema;
                value = { inlined = A { value = 457; param = () } };
                expected = [%ezjsonm { value = 457 }];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled = true;
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
              };
          ];
      }

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

    let inline_nested_optional =
      let inline_nested_optional =
        Alcotest.testable pp_inline_nested_optional equal_inline_nested_optional
      in
      let mk_test name value expected =
        Test
          {
            name;
            here = [%here];
            decoded = None;
            value_testable = inline_nested_optional;
            schema = inline_nested_optional_schema;
            value;
            expected;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled = true;
            swift_class_name = None;
            is_swift_roundtrip_enabled = true;
          }
      in
      {
        name = "inline-nested-optional";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            mk_test "optional inline filled"
              { inline_nested = { inlined = Some { b = 665; c = 666 } } }
              [%ezjsonm { b = 665; c = 666 }];
            mk_test "optional inline empty"
              { inline_nested = { inlined = None } }
              (`O []);
          ];
      }

    let inline_nested_default =
      let inline_nested_default =
        Alcotest.testable pp_inline_nested_default equal_inline_nested_default
      in

      let mk_test name value expected =
        Test
          {
            name;
            here = [%here];
            decoded = None;
            value_testable = inline_nested_default;
            schema = inline_nested_default_schema;
            value;
            expected;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled = true;
            swift_class_name = None;
            is_swift_roundtrip_enabled = true;
          }
      in
      {
        name = "inline-nested-default";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            mk_test "default inline filled"
              { inline_nested = { inlined = Some { b = 665; c = 666 } } }
              [%ezjsonm { b = 665; c = 666 }];
            mk_test "default inline empty"
              { inline_nested = { inlined = None } }
              (`O []);
          ];
      }
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
    Ptime.of_rfc3339 s
    |> Result.ok
    |> Option.value_exn ?here:None ?error:None ?message:None
    |> fun (t, _, _) -> t

  let make_time_local s =
    Ptime.of_rfc3339 s
    |> Result.ok
    |> Option.value_exn ?here:None ?error:None ?message:None
    |> fun (t, tz, _) -> (t, Option.value_exn tz)

  let time =
    {
      name = "time";
      speed = `Quick;
      additional_test = None;
      tests =
        ([
           Test
             {
               name = "time";
               here = [%here];
               decoded = None;
               value_testable = test_time;
               schema = event_schema;
               value =
                 {
                   start = make_time "2020-05-08T23:48:34+02:00";
                   endd_ = make_time "2020-05-08T23:48:47+02:00";
                 };
               expected =
                 [%ezjsonm
                   {
                     start = "2020-05-08T21:48:34-00:00";
                     endd = "2020-05-08T21:48:47-00:00";
                   }];
               kotlin_class_name = None;
               is_kotlin_roundtrip_enabled = true;
               swift_class_name = None;
               is_swift_roundtrip_enabled = true;
             };
         ]
        @
        if Coding.local_time_support then
          [
            Test
              {
                name = "time-local";
                here = [%here];
                decoded = None;
                value_testable = test_time_local;
                schema = event_local_schema;
                value =
                  {
                    start_local = make_time_local "2020-05-08T23:48:34+02:00";
                    end_local = make_time_local "2020-05-08T23:48:47+02:00";
                  };
                expected =
                  [%ezjsonm
                    {
                      start_local = "2020-05-08T23:48:34+02:00";
                      end_local = "2020-05-08T23:48:47+02:00";
                    }];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled = true;
                swift_class_name = None;
                is_swift_roundtrip_enabled = true;
              };
          ]
        else []);
    }

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

  let optional_inline =
    let t = Alcotest.testable pp_optional_inline_wrapper ( = ) in

    let mk_test name value expected =
      Test
        {
          name;
          here = [%here];
          decoded = None;
          value_testable = t;
          schema = optional_inline_wrapper_schema;
          value;
          expected;
          kotlin_class_name = None;
          is_kotlin_roundtrip_enabled = true;
          swift_class_name = None;
          is_swift_roundtrip_enabled = true;
        }
    in
    {
      name = "optional-inline";
      speed = `Quick;
      additional_test = None;
      tests =
        [
          mk_test "missing optional inline" { inline = None; c = 377 }
            [%ezjsonm { c = 377 }];
          mk_test "filled optional inline"
            {
              inline =
                Some { a = 375; b = 376; inline2 = { d = 378 }; missing = None };
              c = 377;
            }
            [%ezjsonm { a = 375; b = 376; d = 378; c = 377 }];
        ];
    }

  type keyedd =
    | Paragraph_ of { content : string } [@schematic.as "paragraph"]
    | Header of {
        content : string;
        level : int;
      }
    | Separator
  [@@schematic.name "keyedd"] (* Check multiple attributes work *)
  [@@schematic.union_key "typ"]
  [@@deriving eq, schema, show]

  let keyed =
    let tester = Alcotest.(testable pp_keyedd equal_keyedd) in

    let mk_test name value expected =
      Test
        {
          name;
          here = [%here];
          decoded = None;
          value_testable = tester;
          schema = keyedd_schema;
          value;
          expected;
          kotlin_class_name = None;
          is_kotlin_roundtrip_enabled = true;
          swift_class_name = None;
          is_swift_roundtrip_enabled = true;
        }
    in
    {
      name = "keyed";
      speed = `Quick;
      tests =
        [
          mk_test "paragraph"
            (Paragraph_ { content = "text" })
            [%ezjsonm { content = "text"; typ = "paragraph" }];
          mk_test "header"
            (Header { content = "text"; level = 411 })
            [%ezjsonm { content = "text"; level = 411; typ = "header" }];
          mk_test "header" Separator [%ezjsonm { typ = "separator" }];
        ];
      additional_test =
        Some
          (fun () ->
            let () =
              let json = [%ezjsonm { content = "text"; level = 411 }] in
              Alcotest.(
                check ~here:[%here]
                  (result tester decoding_error)
                  "values are rejected if the key is missing"
                  (make_error ~schema:"keyedd" {|missing key field "typ"|})
                  (Schematic.Json.decode keyedd_schema json))
            and () =
              let json =
                [%ezjsonm { content = "text"; level = 411; typ = "fumble" }]
              in
              Alcotest.(
                check ~here:[%here]
                  (result tester decoding_error)
                  "values are rejected if the key not recognized"
                  (make_error ~schema:"keyedd"
                     {|invalid key field "typ" = "fumble"|})
                  (Schematic.Json.decode keyedd_schema json))
            and () =
              let json = [%ezjsonm { typ = "header"; content = "text" }] in
              Alcotest.(
                check ~here:[%here]
                  (result tester decoding_error)
                  "errors are specific to the matched case schema"
                  (make_error ~schema:"keyedd"
                     {|missing required field "level"|})
                  (Schematic.Json.decode keyedd_schema json))
            in
            ());
    }

  let map_error =
    let schema =
      Schematic.Schema.make
        (Map
           {
             decode = (fun s -> Result.fail s);
             encode = Fn.id;
             descriptor = String;
           })
    in
    {
      name = "map-error";
      speed = `Quick;
      tests = [];
      additional_test =
        Some
          (fun () ->
            Alcotest.(
              check ~here:[%here]
                (result string decoding_error)
                "errors from Map descriptor decoding are forwarded properly"
                (make_error "whatever")
                (Schematic.Json.decode schema (`String "whatever"))));
    }

  (* JSON Schema metadata *)

  module Metadata = struct
    let test_schema name schema expected =
      let expected =
        match Schematic.Json.of_string expected with
        | Result.Ok json -> json
        | Result.Error _ -> Alcotest.fail "invalid literal JSON"
      and effective = json_schema schema in
      {
        name;
        speed = `Quick;
        tests = [];
        additional_test =
          Some
            (fun () ->
              Alcotest.check ~here:[%here] json "json-schema" expected effective);
      }

    type description = {
      field_a : int; [@schematic.properties { description = "Describe A" }]
      field_b : int; [@schematic.properties { description = "Describe B" }]
    }
    [@@deriving schema]

    let description =
      test_schema "json-schema-metadata-description" description_schema
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

    let default =
      let test_description =
        test_schema "json-schema-metadata-default" default_schema
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
       }|}
      in
      let mk_test name value expected =
        Test
          {
            name;
            here = [%here];
            decoded = None;
            value_testable = Alcotest.(testable pp_default ( = ));
            schema = default_schema;
            value;
            expected;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled = true;
            swift_class_name = None;
            is_swift_roundtrip_enabled = true;
          }
      in

      {
        test_description with
        tests =
          [
            mk_test "default-int" { a = 358; b = "---" }
              [%ezjsonm { b = "---" }];
            mk_test "default-string" { a = 359; b = "359" }
              [%ezjsonm { a = 359 }];
            mk_test "default-both" { a = 358; b = "359" } (`O []);
            mk_test "default-none" { a = 359; b = "---" }
              [%ezjsonm { a = 359; b = "---" }];
          ];
      }
  end

  module Comparison = struct
    open Alcotest

    let check ~here name schema e l r =
      check ~here int name e (Schematic.Schema.compare schema l r)

    let bool =
      let check = check "bool" bool_schema in
      {
        name = "bool";
        speed = `Quick;
        tests = [];
        additional_test =
          Some
            (fun () ->
              let () = check ~here:[%here] 0 true true in
              let () = check ~here:[%here] 0 false false in
              let () = check ~here:[%here] 1 true false in
              let () = check ~here:[%here] (-1) false true in
              ());
      }

    let int =
      let check = check "int" int_schema in
      {
        name = "int";
        speed = `Quick;
        tests = [];
        additional_test =
          Some
            (fun () ->
              let () = check ~here:[%here] 0 0 0 in
              let () = check ~here:[%here] (-1) 0 1 in
              let () = check ~here:[%here] 1 1 0 in
              ());
      }

    let string =
      let check = check "string" string_schema in
      {
        name = "string";
        speed = `Quick;
        tests = [];
        additional_test =
          Some
            (fun () ->
              let () = check ~here:[%here] 0 "a" "a" in
              let () = check ~here:[%here] (-1) "a" "b" in
              let () = check ~here:[%here] 1 "b" "a" in
              let () = check ~here:[%here] 1 "aa" "a" in
              let () = check ~here:[%here] (-1) "a" "aa" in
              ());
      }

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

    let record =
      let check = check "version" version_schema in
      {
        name = "record";
        speed = `Quick;
        tests = [];
        additional_test =
          Some (fun () -> versions (check ~here:[%here]) version);
      }

    let tuple =
      let check = check "version" version_tuple_schema in
      {
        name = "tuple";
        speed = `Quick;
        tests = [];
        additional_test = Some (fun () -> versions (check ~here:[%here]) Fn.id);
      }

    let variant =
      let check = check "variant" expectation_schema
      and v = { major = 1; minor = 2; subminor = 3 } in
      {
        name = "variant";
        speed = `Quick;
        tests = [];
        additional_test =
          Some
            (fun () ->
              let () = check ~here:[%here] 0 (Exact v) (Exact v)
              and () = check ~here:[%here] (-1) (Exact v) Nothing
              and () = check ~here:[%here] 1 Nothing (Exact v) in
              ());
      }
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

    let missing =
      {
        name = "missing";
        speed = `Quick;
        tests = [];
        additional_test =
          Some
            (fun () ->
              let () =
                test ~here:[%here] "missing fields are reported"
                  Recursive.schema {|{"value":0,"next":{"value":1,"next":{}}}|}
                  {|path next.next: schema recursive: missing required field "value"|}
              and () =
                test ~here:[%here] "root error display no path" Recursive.schema
                  {|{}|} {|schema recursive: missing required field "value"|}
              in
              ());
      }

    let type_error =
      {
        name = "type error";
        speed = `Quick;
        tests = [];
        additional_test =
          Some
            (fun () ->
              test ~here:[%here] "missing" Recursive.schema
                {|{"value":0,"next":{"value":1,"next":{"value":"nope"}}}|}
                {|path next.next.value: expected integer, got "nope"|});
      }
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

    let deduced =
      {
        name = "deduced names";
        speed = `Quick;
        tests = [];
        additional_test =
          Some
            (fun () ->
              Alcotest.(
                check ~here:[%here] (option string)
                  "schema name is deduced from type name" (Some "foo")
                  foo_schema.id);
              Alcotest.(
                check ~here:[%here] (option string)
                  "schema name is deduced from type module name when type is \
                   \"t\""
                  (Some "names") schema.id);
              Alcotest.(
                check ~here:[%here] (option string)
                  "schema name is deduced from file name when type is \"t\""
                  (Some "tests") root_schema.id));
      }

    type 'a result =
      | Ok of 'a
      | Error
    [@@deriving schema]

    let parametric =
      {
        name = "parametric schemas names";
        speed = `Quick;
        tests = [];
        additional_test =
          Some
            (fun () ->
              Alcotest.(
                check ~here:[%here] (option string)
                  "schema name is deduced from type name" (Some "foo-result")
                  (result_schema foo_schema).id);
              Alcotest.(
                check ~here:[%here] (option string)
                  "schema name is deduced from type module name when type is \
                   \"t\""
                  (Some "names-result") (result_schema schema).id));
      }
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

  let collision =
    {
      name = "collisions";
      speed = `Quick;
      additional_test = None;
      tests =
        [
          Test
            {
              name = "collision_1";
              here = [%here];
              decoded = None;
              value_testable = collision_1;
              schema = collision_1_schema;
              value = ({ collision_a = 725; collision_b = "725" } : collision_1);
              expected = [%ezjsonm { collision_a = 725; collision_b = "725" }];
              kotlin_class_name = None;
              is_kotlin_roundtrip_enabled = true;
              swift_class_name = None;
              is_swift_roundtrip_enabled = true;
            };
          Test
            {
              name = "collision_2";
              here = [%here];
              decoded = None;
              value_testable = collision_2;
              schema = collision_2_schema Schematic.Schemas.string_schema;
              value =
                ({ collision_a = 725; collision_b = "725" }
                  : string collision_2);
              expected = [%ezjsonm { collision_a = 725; collision_b = "725" }];
              kotlin_class_name = None;
              is_kotlin_roundtrip_enabled = true;
              swift_class_name = None;
              is_swift_roundtrip_enabled = true;
            };
        ];
    }

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

    let computed =
      let mk_test name value expected =
        Test
          {
            name;
            here = [%here];
            decoded = None;
            value_testable = t;
            schema;
            value;
            expected;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled = true;
            swift_class_name = None;
            is_swift_roundtrip_enabled = true;
          }
      in
      {
        name = "computed";
        speed = `Quick;
        tests =
          [
            mk_test "computed" { x = 1; y = 2 }
              [%ezjsonm { x = 1; y = 2; sum = 3; opposite = -1 }];
            mk_test "computed optional" { x = 0; y = 0 }
              [%ezjsonm { x = 0; sum = 0 }];
          ];
        additional_test =
          Some
            (fun () ->
              (* This used to fail because the computed field was expected *)
              Alcotest.(check ~here:[%here] (result t decoding_error))
                "variant containing records with missing computed field"
                (Result.Ok { x = 1198; y = 33 })
              @@ Schematic.Json.decode schema [%ezjsonm { x = 1198; y = 33 }]);
      }
  end

  (* Version *)

  module Versionn = struct
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
      v : v Opt.t;
          [@schematic.schema legacy < "0.6.1"] [@schematic.as "w" < "0.6.1"]
    }
    [@@deriving eq, schema, show]

    let t = Alcotest.testable pp equal

    let schema_versions = function
      | Schemas_library.V -> Some (Schematic.Version.make 0 6 0)
      | _ -> None

    let test =
      let mk_test ?decoded name schema value expected
          is_kotlin_roundtrip_enabled =
        Test
          {
            name;
            here = [%here];
            decoded;
            value_testable = t;
            schema;
            value;
            expected;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled;
            swift_class_name = None;
            is_swift_roundtrip_enabled = is_kotlin_roundtrip_enabled;
          }
      in
      {
        name = "version";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            mk_test "latest none" schema { v = None } [%ezjsonm { v = None }]
              true;
            mk_test "latest int" schema { v = Some (Int 3) }
              [%ezjsonm { v = 3 }] true;
            mk_test "latest string" schema { v = Some (String "3") }
              [%ezjsonm { v = "3" }] true;
            (* Kotlin roundtrip tests disabled for the old ones, since the data
               type uses the latest one *)
            mk_test "old none"
              (schema_versioned schema_versions)
              { v = None } [%ezjsonm { w = None }] false;
            mk_test "old int"
              (schema_versioned schema_versions)
              { v = Some (Int 3) } [%ezjsonm { w = 3 }] false;
            mk_test "old string"
              (schema_versioned schema_versions)
              { v = Some (String "3") } [%ezjsonm { w = None }] false
              ~decoded:{ v = None };
          ];
      }
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

    let test =
      let test_latest =
        let value = { id = "glouglou"; content = { content = "foo" } } in
        let value = { t_ = value } in
        let json =
          [%ezjsonm { id = "glouglou"; content = { content_new = "foo" } }]
        in
        Test
          {
            name = "latest";
            here = [%here];
            decoded = None;
            value_testable = testable;
            schema;
            value;
            expected = json;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled = true;
            swift_class_name = None;
            is_swift_roundtrip_enabled = true;
          }
      in
      let test_legacy =
        let schema_versions = function
          | Schemas_library.V -> Some (Schematic.Version.make 0 9 0)
          | _ -> None
        in
        let value = { id = "glouglou"; content = { content = "foo" } } in
        let value = { t_ = value } in
        let json = [%ezjsonm { content_old = "foo" }] in
        let schema = schema_versioned schema_versions in
        Test
          {
            name = "legacy";
            here = [%here];
            decoded = None;
            value_testable = testable;
            schema;
            value;
            expected = json;
            kotlin_class_name = None;
            is_kotlin_roundtrip_enabled =
              false
              (* Disabled since the Kotlin data type uses the latest schema *);
            swift_class_name = None;
            is_swift_roundtrip_enabled = false;
          }
      in
      {
        name = "version_cascade";
        speed = `Quick;
        additional_test = None;
        tests = [ test_latest; test_legacy ];
      }
  end

  module VersionSince = struct
    module T = struct
      type t =
        | A of int
        | B of string
        | C of bool [@schematic.since "0.21.4"]
      [@@deriving eq, show, schema]
    end

    include T

    let since =
      Test
        {
          here = [%here];
          decoded = None;
          name = "since";
          value_testable = (module T);
          schema;
          value = C true;
          expected = [%ezjsonm true];
          kotlin_class_name = None;
          is_kotlin_roundtrip_enabled = false;
          swift_class_name = None;
          is_swift_roundtrip_enabled = false;
        }

    let test =
      {
        name = "version_since";
        speed = `Quick;
        additional_test = None;
        tests = [ since ];
      }
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

    let pp_schema =
      {
        name = "pp schema";
        speed = `Quick;
        tests = [];
        additional_test =
          Some
            (fun () ->
              Alcotest.(check ~here:[%here] text)
                "schema pretty print"
                {|<graph-result> =
Union {
  "list": { schema: [ <graph-result> ] },
  "nullable": { schema: Nullable (<graph-result>) },
  "object": { schema: { value (Required): <graph-result> } },
  "tuple": { schema: [ <graph-result>, <graph-result>,  ] }
}|}
              @@ Fmt.str "%a" Schematic.Schema.pp
              @@ T.schema_versioned (fun _ -> None));
      }

    let pp_version = Schematic.Pp.of_schema version_schema
    let pp_range = Schematic.Pp.of_schema range_schema

    let pp =
      {
        name = "pp";
        speed = `Quick;
        tests = [];
        additional_test =
          Some
            (fun () ->
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
                  (Buffer.contents buffer)));
      }
  end

  module Extension = struct
    let atomic =
      {
        name = "atomic";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            Test
              {
                name = "integer";
                here = [%here];
                decoded = None;
                value_testable = Alcotest.int;
                schema = [%schema: int];
                value = 42;
                expected = [%ezjsonm 42];
                kotlin_class_name = Some "java.lang.Integer";
                is_kotlin_roundtrip_enabled = true;
                swift_class_name = Some "Swift.Int";
                is_swift_roundtrip_enabled = true;
              };
          ];
      }

    let composite =
      {
        name = "composite";
        speed = `Quick;
        additional_test = None;
        tests =
          [
            Test
              {
                name = "pair";
                here = [%here];
                decoded = None;
                value_testable = Alcotest.(pair int string);
                schema = [%schema: int * string];
                value = (42, "forty two");
                expected = [%ezjsonm [ 42; "forty two" ]];
                kotlin_class_name = None;
                is_kotlin_roundtrip_enabled =
                  false
                  (* Kotlin [Class.forName] does not support generics like
                     [Pair<Integer, String>] *);
                swift_class_name = None;
                is_swift_roundtrip_enabled = false;
              };
          ];
      }

    (* Not really a runtime test, just making sure that this doesn't raise an
       `unused-open` warning *)
    let _ = [%schema: t]
  end

  module Documentation = struct
    (** This is a documented type for testing *)
    type documented_type = int [@@deriving schema]

    type undocumented_type = int [@@deriving schema]

    (** A record with documentation *)
    type documented_record = {
      field1 : int;
      field2 : string;
    }
    [@@deriving schema]

    (** A variant with documentation *)
    type documented_variant =
      | A
      | B of int
    [@@deriving schema]

    (** A user with documented fields *)
    type user_with_field_docs = {
      name : string;  (** The user's full name *)
      age : int;  (** Age in years *)
      email : string option;  (** Optional email address *)
    }
    [@@deriving schema]

    (** A result type with documented cases *)
    type result_with_case_docs =
      | Ok of int  (** Success with a value *)
      | Error of string  (** Failure with an error message *)
    [@@deriving schema]

    let basic =
      {
        name = "basic documentation";
        speed = `Quick;
        tests = [];
        additional_test =
          Some
            (fun () ->
              Alcotest.(
                check ~here:[%here] (option string)
                  "documented type has documentation"
                  (Some "This is a documented type for testing")
                  documented_type_schema.documentation);
              Alcotest.(
                check ~here:[%here] (option string)
                  "undocumented type has no documentation" None
                  undocumented_type_schema.documentation));
      }

    let record =
      {
        name = "record documentation";
        speed = `Quick;
        tests = [];
        additional_test =
          Some
            (fun () ->
              Alcotest.(
                check ~here:[%here] (option string)
                  "documented record has documentation"
                  (Some "A record with documentation")
                  documented_record_schema.documentation));
      }

    let variant =
      {
        name = "variant documentation";
        speed = `Quick;
        tests = [];
        additional_test =
          Some
            (fun () ->
              Alcotest.(
                check ~here:[%here] (option string)
                  "documented variant has documentation"
                  (Some "A variant with documentation")
                  documented_variant_schema.documentation));
      }

    let field_docs =
      {
        name = "field documentation";
        speed = `Quick;
        tests =
          [
            Test
              {
                name = "field documentation";
                here = [%here];
                decoded = None;
                schema = user_with_field_docs_schema;
                kotlin_class_name = Some "UserWithFieldDocs";
                is_kotlin_roundtrip_enabled = false;
                swift_class_name = None;
                is_swift_roundtrip_enabled = false;
                value =
                  { name = "Alice"; age = 30; email = Some "alice@example.com" };
                expected =
                  [%ezjsonm
                    { name = "Alice"; age = 30; email = "alice@example.com" }];
                value_testable =
                  Alcotest.testable
                    (fun f { name; age; email } ->
                      Fmt.pf f "{ name = %s; age = %d; email = %a }" name age
                        Fmt.(option string)
                        email)
                    (fun a b ->
                      String.(a.name = b.name)
                      && Int.(a.age = b.age)
                      && Option.equal String.equal a.email b.email);
              };
          ];
        additional_test = None;
      }

    let case_docs =
      {
        name = "case documentation";
        speed = `Quick;
        tests =
          [
            Test
              {
                name = "case documentation";
                here = [%here];
                decoded = None;
                schema = result_with_case_docs_schema;
                kotlin_class_name = Some "ResultWithCaseDocs";
                is_kotlin_roundtrip_enabled = false;
                swift_class_name = None;
                is_swift_roundtrip_enabled = false;
                value = Ok 42;
                expected = `Float 42.0;
                value_testable =
                  Alcotest.testable
                    (fun f -> function
                      | Ok v -> Fmt.pf f "Ok %d" v
                      | Error s -> Fmt.pf f "Error %s" s)
                    (fun a b ->
                      match (a, b) with
                      | Ok x, Ok y -> Int.(x = y)
                      | Error x, Error y -> String.(x = y)
                      | _ -> false);
              };
          ];
        additional_test = None;
      }
  end

  (* Driver *)

  let test_descriptions : test_group list =
    [
      {
        name = "coding";
        test_cases =
          [
            atom_int;
            atom_int64;
            atom_float;
            atom_string;
            atom_date;
            coding;
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
            alias;
            List.objects;
            List.option;
            Mapping.objects;
            Parametric.simple;
            Parametric.anonymous;
            Gadt.simple;
            Gadt.complex;
            escape;
            Recursive.plain;
            Recursive.parametric;
            mutually;
            ignored;
            Inline.record;
            Inline.variant_payload;
            Inline.inline_variant;
            Inline.inline_try;
            Inline.inline_nested_union_required;
            Inline.inline_nested_union_optional;
            Inline.inline_parametric;
            Inline.inline_nested_optional;
            Inline.inline_nested_default;
            time;
            Metadata.description;
            Metadata.default;
            failure;
            optional_inline;
            keyed;
            map_error;
            collision;
            Computed.computed;
            Versionn.test;
            VersionCascade.test;
            VersionSince.test;
          ];
      };
      {
        name = "comparison";
        test_cases =
          [
            Comparison.bool;
            Comparison.int;
            Comparison.string;
            Comparison.record;
            Comparison.tuple;
            Comparison.variant;
          ];
      };
      {
        name = "error-messages";
        test_cases = [ Error_messages.missing; Error_messages.type_error ];
      };
      { name = "names"; test_cases = [ Names.deduced; Names.parametric ] };
      { name = "pp"; test_cases = [ Pp.pp; Pp.pp_schema ] };
      {
        name = "extension";
        test_cases = [ Extension.atomic; Extension.composite ];
      };
      {
        name = "documentation";
        test_cases =
          [
            Documentation.basic;
            Documentation.record;
            Documentation.variant;
            Documentation.field_docs;
            Documentation.case_docs;
          ];
      };
    ]

  let tests : (string * unit Alcotest.test_case list) list =
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
end

module Schemas_library = Entropic.Schemas_library

module Entropy = struct
  type t = Cryptokit.Random.rng

  let get rng len =
    let buf = Bytes.create len in
    let () = rng#random_bytes buf 0 len in
    (rng, Some buf)
end

module Entropic = Entropic.Make (Entropy)

let entropy = Cryptokit.Random.pseudo_rng "entropicentropic"

let bool () =
  Alcotest.(check ~here:[%here] (result pass string)) "bool" (Result.Ok true)
  @@ Entropic.generate entropy Schematic.Schemas.bool_schema

let float () =
  Alcotest.(check ~here:[%here] (result pass string)) "float" (Result.Ok 0.)
  @@ Entropic.generate entropy Schematic.Schemas.float_schema

let int () =
  Alcotest.(check ~here:[%here] (result pass string)) "int" (Result.Ok 0)
  @@ Entropic.generate entropy Schematic.Schemas.int_schema

let int64 () =
  Alcotest.(check ~here:[%here] (result pass string)) "int64" (Result.Ok 0L)
  @@ Entropic.generate entropy Schematic.Schemas.int64_schema

let string () =
  Alcotest.(check ~here:[%here] (result pass string)) "string" (Result.Ok "")
  @@ Entropic.generate entropy Schematic.Schemas.string_schema

let list () =
  Alcotest.(check ~here:[%here] (result pass string)) "int list" (Result.Ok [])
  @@ Entropic.generate entropy Schematic.Schemas.(list_schema int_schema)

type o = {
  x : int;
  y : int;
}
[@@schematic.computed { sum : int = x + 1 }] [@@deriving schema]

let obj () =
  Alcotest.(check ~here:[%here] (result pass string))
    "object"
    (Result.Ok { x = 0; y = 0 })
  @@ Entropic.generate entropy o_schema

type o_composite = { y : y }

and y =
  | Foo of int list
  | Bar
[@@deriving schema]

let small_size () =
  Alcotest.(check ~here:[%here] (result pass string))
    "object"
    (Result.Ok { y = Bar })
  @@ Entropic.generate ~size:0 entropy o_composite_schema

type nested_list = Foo of nested_list list [@@deriving schema]

let nested_list () =
  Alcotest.(check ~here:[%here] (result pass string))
    "nested lists" (Result.Ok (Foo []))
  @@ Entropic.generate entropy nested_list_schema

let () =
  let test name f = Alcotest.test_case name `Quick f in
  Alcotest.run "Entropic"
    [
      ( "atoms",
        [
          test "bool" bool;
          test "float" float;
          test "int" int;
          test "int64" int64;
          test "string" string;
        ] );
      ( "composite",
        [ test "list" list; test "object" obj; test "small_size" small_size ] );
      ("recursive", [ test "nested lists" nested_list ]);
    ]

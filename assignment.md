# Backend engineer test assignment

This is not a "code exercice on your own in X hours" kind of assignment. The whole point is to assess how well we'd work together, so consider I'm your colleague for the duration of this exercice, as if these were just tickets. Feel free to message me with any question regarding schematic or the ecosystem, or ask for directions if you're lost.

The assignment will take place in schematic, a library defining schemas that describe types and enables type-based meta-programming; see `README.md` for a very brief introduction. It is accompanied with a PPX deriver to avoid manually writing schemas for types defined with `type _ = ...`.

Schematic is not the cleanest piece of code we have, as it is at the root of everything we do and has gone through many iteration and mutation over the years. It's therefore a good example of what a compromise between perfect craftsmanship and iteration speed can look like at Routine. Don't expect everything to be documented and cleanly refactored, you will need some exploration, inference and asking around - it's still in pretty good shape and well tested, don't worry too much.

## Material

You will be provided with a custom fork of the schematic library. The assignment only relies on `schematic` and `schematic-jsoo`, so it's probably easier to only install those. Setting things up should be as straightforward as:

```ocaml
# From within the cloned repository
opam switch create . --no-install # Don't install all repo packages, just a compiler
yarn # The test suites checks against the javascript JSON schema implementation
opam install ./schematic.opam --with-test # Install only schematic
opam install ./schematic-jsoo.opam --with-test # Install only schematic-jsoo
dune runtest -p schematic,schematic-jsoo # Run only schematic and schematic-jsoo unit-tests
```

Please format your code using `ocamlformat`.

## Assignment 1: new % schema extension point

### Context

The schematic library defines schemas for OCaml types, which are meta-information describing the type structure and enable many meta-programming instrumentation such as serialization. While it would be possible to write these schemas manually for each type, this would be quite tedious, error prone and a maintenance nightmare. For this reason, schematic comes with a [ppx](https://ocaml.org/docs/metaprogramming) that automatically derives a schema for types.

```ocaml
type xxx = { x: int; y: int } [@@deriving schema]

(* The derivation expands to the following (simplified for readability): *)

let xxx_schema_versioned (__schematic_version : Schematic.Schema.version) =
  let descriptor =
    Schema.Object
      {
        decode = (fun Hlist.[ x; y ] : xxx -> { x; y });
        encode =
          (fun ({ x; y } : xxx) ->
            let open Hlist in
            [ x; y ]);
        fields =
          Field
            {
              field =
                {
                  name = "x";
                  requirement = Required;
                  schema = Outline (int_schema_versioned __schematic_version);
                };
              rest =
                Field
                  {
                    field =
                      {
                        name = "y";
                        requirement = Required;
                        schema =
                          Outline (int_schema_versioned __schematic_version);
                      };
                    rest = FieldEnd;
                  };
            };
      }
  and id = Some "xxx" in
  { Schema.descriptor; id }

let xxx_schema : xxx Schema.t = xxx_schema_versioned (fun _ -> None)
```

To sum up, it defines a `xxx_schema_versioned` function that return a schema for the `xxx` type given some versioning information, which enable to adapt serialization depending on version for backward compatibility. The version argument is a function that takes a library identifier and returns the version to use to serialize types from that library, or `None` to use the latest version. There is no need to go into further details regarding this for this assignment, other than knowing that the `fun _ -> None` function thus selects the latest version of all schemas.

 It also defines a convenience `xxx_schema` value, which is the schema at the latest possible version.

### Assignment

This deriver is very convenient, but we sometime end up in a situation where we need the schema for a core type that has no separate lexical definition to attach our `@@deriving` attribute to.

```ocaml
let to_json (v : int * string) =
  (* [some_schema] is unfortunately undefined here *)
  Schematic.Json.encode some_schema v
```

In the above example, we want to obtain a schema for `int * string`. The way we achieve this currently is often with a dummy type declaration.

```ocaml
type int_string_pair = int * string [@@deriving schema]
(* Just to obtain the schema really *)

let to_json v =
  Schematic.Json.encode int_string_pair_schema v
```

While this works reasonably well, it is still quite verbose and clutters the namespace with useless types. A better solution would be to create a ppx extension point, which would enable writing:

```ocaml
let to_json v =
  Schematic.Json.encode [%schema: int * string] v
```

Providing such an extension point is your assignment.

### Guidelines

The deriver was written with type declaration in mind, and some hypothesis are made in that direction, such as types having names. You may use a dummy name for now, as refactoring the whole thing to accommodate for this properly would be way too big a task.

Most functions to derive schema of types are of course already present, you won't have to write any such code, this is mostly about plumbing things the right way. You will probably notice that the functions will yield versioned schemas, which expect a versioning function as described earlier. For `[%schema]` extension points, let's always yield the schema for the latest version, so using `fun _ -> None` an the version function. If you fancy, you can introduce `[%schema-versioned]` extension point that yields the versioned schema.

## Assignment 2: Strange bug with the js_of_ocaml binding

### Context

The web frontend devs reported a strange stack overflow in the app. I bisected and managed to extract a minimal reproduction test in `schematic-jsoo/tests/jsoo.ml`.

### Assignment

Find and fix the bug.

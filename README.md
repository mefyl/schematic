# Schematic

![Master build status](https://gitlab.routine.co/routine/schematic/badges/master/pipeline.svg)

Derive schema from types.

```ocaml
type version =
  { major : int
  ; minor : int
  ; subminor : int
  } [@@deriving schema]
```

## Coding

Encode and decode to different formats using said schema.

```ocaml
let json =
  Json.encode version_schema {major = 1; minor = 2; subminor = 3}
  |> Json.to_string

val json : string = "{\"major\":1,\"minor\":2,\"subminor\":3}"

let version =
  Json.of_string json
  |> Json.decode version_schema

val version : version = {major = 1; minor = 2; subminor = 3}
```

Encode the schema itself to different formats.

```ocaml
let () = Json.of_schema version_schema |> Format.printf "%a" Json.pp

{"$schema":
   "http://json-schema.org/draft-04/schema#",
   "type":
   "object",
   "properties":
   {"major":
      {"type":
         "integer",
         "minimum":
         -1073741824.000000,
         "maximum":
         1073741823.000000},
      "minor":
      {"type":
         "integer",
         "minimum":
         -1073741824.000000,
         "maximum":
         1073741823.000000},
      "subminor":
      {"type":
         "integer",
         "minimum":
         -1073741824.000000,
         "maximum":
         1073741823.000000}},
   "required":
   ["subminor", "minor", "major"],
   "additionalProperties":
   false}
```

## Versioning

### Schemas versions

Schema can be versioned and have their coded representation evolve
with new versions. When using the `*_versioned` flavor of a schema and
passing it a versioning argument, schemas can decode from or encode to
an older version. This can be useful to read data that is encoded at
rest with an older version, provided the encoding version was
registered next to it. It can also enable older client code to
interact with the latest version, provided that client code can
declare what version of the schemas it's using. It is then possible to
decode incoming data and encode outbound data back at the right
version to serve multiple older client code.

For instance, suppose that a field was once erroneously called
`"reccurence"`. We can fix its name in version 1.2.3 using:

```
type task = {
  recurrence : ...; [@schematic.as "reccurence" < "1.2.3"]
}
```
let task_schema_versioned 

Passing `(fun V -> Some (Version.make 1 2 2) | _ -> None)` to
`task_schema_versioned` will produce a schema that read and write that
fields using the old name, enabling interaction with older versions,
be it data encoded at rest or an older client over the network.

### Libraries version

Multiple different libraries may declare schemas, and they may all
want to version them and thus need their own versions track. To
accomodate this, schemas are grouped under the banner of a library
through the `Library_schemas.V` flag of type `Schema.library`, which
is an extensible tag. It entails that `_versioned` routines are not
passed a single version, but a `Schema.library -> Version.t option`
argument which can determine what coding version to use for schemas
from this specific library, `None` meaning "the latest". Non
`_versioned` flavors of function simply always pass `fun _ -> None`,
so that if your use case requires no versioning, you can mostly ignore
this whole matter.

### Minimal decoding version

Schematic supports declaring the minimal required version to decode a
value through annotations. When encoding, using `encode_versioned`
instead of `encode` will yield an additional map that describes the
minimal version of each library required to decode the result. This
can be used for instance to determine later whether client code has
recent enough schemas to handle this serialized piece of data.

Supported means of declaring a minimal version:

* `@schematic.since` on variant constructors declares that this
  constructor was introduced at a specific version.
  
  ```ocaml
  type t =
    | A of int
    | B of string [@@schematic.since 1.2.3] 
      (** clients older than 1.2.3 will not be able to decode [B]. *)
  ```

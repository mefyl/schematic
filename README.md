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

### Minimal decoding version

Schematic supports declaring the minimal required version to decode an
object through annotations. When encoding an object, using
`encode_versioned` instead of `encode` will yield an additional map
that describes the minimal version of each library required to decode
the result. This can be used for instance to determine later whether a
client is recent enough to receive this serialized piece of data.

Supported means of declaring a minimal version:

* `@schematic.since` on variant constructors declares that this
  constructor was introduced at a specific version.
  
  ```ocaml
  type t =
    | A of int
    | B of string [@@schematic.since 1.2.3] 
      (** clients older than 1.2.3 will not be able to decode [B]. *)
  ```

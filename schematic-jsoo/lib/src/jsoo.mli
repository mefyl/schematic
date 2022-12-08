include module type of Jsoo_intf
include Schematic.Coding.S with type t = Js_of_ocaml.Js.Unsafe.any

val helpers :
  string ->
  'a Schematic.Schema.t ->
  ('a -> t) * (t -> ('a, Schematic.Error.decoding) result) * (t -> 'a)

val helpers_typed :
  (module JS with type t = 't and type js = 'js Js_of_ocaml.Js.t) ->
  string ->
  't Schematic.Schema.t ->
  ('t -> 'js Js_of_ocaml.Js.t)
  * ('js Js_of_ocaml.Js.t -> ('t, Schematic.Error.decoding) result)
  * ('js Js_of_ocaml.Js.t -> 't)

val schema_versioned : Schematic.Schema.version -> t Schematic.Schema.t
val schema : t Schematic.Schema.t

type js = t

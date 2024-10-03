include Schematic.Json.S

val schema : t Schematic.Schema.t

module String : Schematic.Coding.STR with type t = string

val of_js : Js_of_ocaml.Js.Unsafe.any -> t
val to_js : t -> Js_of_ocaml.Js.Unsafe.any

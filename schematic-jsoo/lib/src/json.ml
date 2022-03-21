module T = Schematic.Json.Make (Impl_js)
include T
module String = Schematic.Coding.Make_string (T)

type _ Schematic.Coding.coding +=
  | Coding : Js_of_ocaml.Js.Unsafe.any Schematic.Coding.coding

module type JS = sig
  type t
  type js
end

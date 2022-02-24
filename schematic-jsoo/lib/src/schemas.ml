include Schematic.Schemas

type bool_js = bool Js_of_ocaml.Js.t
type int_js = Js_of_ocaml.Js.number Js_of_ocaml.Js.t
type float_js = Js_of_ocaml.Js.number Js_of_ocaml.Js.t
type any_js = Js_of_ocaml.Js.Unsafe.any
type unit_js = Js_of_ocaml.Js.Unsafe.any Js_of_ocaml.Js.Opt.t
type ignore_js = Js_of_ocaml.Js.Unsafe.any

module Either = struct
  include Either

  type (_, _) js = Js_of_ocaml.Js.Unsafe.any
end

module Result = struct
  include Result

  type (_, _) js = Js_of_ocaml.Js.Unsafe.any
end

module Ptime = struct
  include Ptime

  type js = Js_of_ocaml.Js.date Js_of_ocaml.Js.t
  type date_js = Js_of_ocaml.Js.date Js_of_ocaml.Js.t
  type span_js = int Js_of_ocaml.Js.t
end

module Uri = struct
  include Uri

  type js = Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t
end

type 'a list_js = 'a Js_of_ocaml.Js.js_array Js_of_ocaml.Js.t
type 'a option_js = 'a Js_of_ocaml.Js.opt
type string_js = Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t

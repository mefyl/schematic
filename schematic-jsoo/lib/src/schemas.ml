module Js = Js_of_ocaml.Js

type bool_js = bool Js.t

type int_js = Js.number Js.t

type any_js = Js.Unsafe.any

type unit_js = Js.Unsafe.any Js.Opt.t

type ignore_js = Js.Unsafe.any

module Either = struct
  include Schematic.Schema.Schemas.Either

  type (_, _) js = Js.Unsafe.any
end

module Result = struct
  include Schematic.Schema.Schemas.Result

  type (_, _) js = Js.Unsafe.any
end

module Ptime = struct
  include Schematic.Schema.Schemas.Ptime

  type js = Js.date Js.t

  type date_js = Js.date Js.t

  type span_js = int Js.t
end

module Uri = struct
  include Schematic.Schema.Schemas.Uri

  type js = Js.js_string Js.t
end

type 'a list_js = 'a Js.js_array Js.t

type 'a option_js = 'a Js.opt

type string_js = Js.js_string Js.t

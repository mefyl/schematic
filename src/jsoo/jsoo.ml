open Js_of_ocaml

type bool_js = bool Js.t

type int_js = int

module Ptime = struct
  include Schematic.Schema.Ptime

  type js = Js.date Js.t

  type date_js = Js.date Js.t
end

type 'a list_js = 'a Js.js_array Js.t

type 'a option_js = 'a Js.opt

type string_js = Js.js_string Js.t

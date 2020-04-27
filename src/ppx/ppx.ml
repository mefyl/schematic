open Ppxlib

let str_type_decl =
  Deriving.Generator.make_noarg ~attributes:[]
    (Schematic_ppx_expand.Expand.expand ~inside:false)

let my_deriver = Deriving.add ~str_type_decl "schema"

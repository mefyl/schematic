open Base
(* Export *)
(*  type sw_gen = { name : Identifier.t; typ : unit Fmt.t; } *)

module Builtins = Swift_builtins

let type_decl (name, c) : string option =
  Fmt.epr "type_decl: %a@." C_intf.pp c;
  Swift_typeexpr.type_decl (name, c)

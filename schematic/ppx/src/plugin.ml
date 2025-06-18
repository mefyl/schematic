open Ppxlib
open Utils

module type S = sig
  (** Key in the [~plugin] deriver argument. *)
  val key : string

  (** Pretty name of the plugin *)
  val name : string

  (** Type of the configuration in the [~plugin] deriver key. *)
  type configuration

  (** Shape of the configuration in the [~plugin] deriver key. *)
  val configuration_pattern :
    unit -> (expression, configuration -> 'a, 'a) Ast_pattern.t

  (** Short description for the plugin. *)
  val synopsis : string

  module Expansion : Expansion.S with type configuration = configuration
end

type t = (module S)

let plugins = ref ([] : t list)
let register plugin = plugins := plugin :: !plugins

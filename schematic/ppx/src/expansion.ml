open Base
open Intf
open Ppxlib

module type S = sig
  type closed
  type configuration
  type expanded

  val close :
    configuration:configuration ->
    context:context ->
    type_context:type_context ->
    expanded ->
    closed

  val combine :
    loc:location -> configuration -> closed list -> structure_item list

  val expand_abstract_type :
    context:context -> type_context:type_context -> core_type -> expanded

  val expand_record :
    context:context ->
    type_context:type_context ->
    Attributes.field list ->
    expanded

  val expand_variant :
    context:context ->
    type_context:type_context ->
    constructor_declaration list ->
    expanded
end

module M (Expansion : S) = struct
  let expand ~configuration ~context types =
    let f (type_, type_context) =
      let body =
        match type_ with
        | Abstract -> failwith "no manifest on abstract type"
        | Core core ->
          Expansion.expand_abstract_type ~context ~type_context core
        | Record fields -> Expansion.expand_record ~context ~type_context fields
        | Variant options ->
          Expansion.expand_variant ~context ~type_context options
      in
      Expansion.close ~configuration ~context ~type_context body
    in
    let expanded = List.map ~f types in
    Expansion.combine ~loc:context.loc configuration expanded
end

open Schema

val bool_schema : bool schema

val int_schema : int schema

val list_schema : 'a schema -> 'a list schema

val mapping_schema : 'a schema -> (string * 'a) list schema

val string_schema : string schema

val string_const_schema : string -> unit schema

val unit_schema : unit schema

module Ptime : sig
  include module type of Ptime with type t = Ptime.t

  val schema : Ptime.t Schema.schema

  (** Unfortunately [Ptime] does not define a standard type for time points with
      timezone and uses tuples all around. Define ours.*)
  type local = Ptime.t * Ptime.tz_offset_s

  val local_schema : local Schema.schema

  val date_schema : date Schema.schema
end

module Ptime : sig
  include module type of Ptime with type t = Ptime.t

  val schema : Ptime.t Schema.schema

  (** Unfortunately [Ptime] does not define a standard type for time points with
      timezone and uses tuples all around. Define ours.*)
  type local = Ptime.t * Ptime.tz_offset_s

  val local_schema : local Schema.schema

  val date_schema : date Schema.schema
end

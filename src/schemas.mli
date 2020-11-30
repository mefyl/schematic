open Schema

val bool_schema : bool schema

val int_schema : int schema

val list_schema : 'a schema -> 'a list schema

val mapping_schema : 'a schema -> (string * 'a) list schema

val string_schema : string schema

val string_const_schema : string -> unit schema

val unit_schema : unit schema

type any = Ezjsonm.value

val any_schema : any schema

module Ptime : sig
  include module type of Ptime with type t = Ptime.t

  val schema : Ptime.t Schema.schema

  (** Unfortunately [Ptime] does not define a standard type for time points with
      timezone and uses tuples all around. Define ours.*)
  type local = Ptime.t * Ptime.tz_offset_s

  val local_schema : local Schema.schema

  val date_schema : date Schema.schema
end

module Uri : sig
  include module type of Uri

  val schema : Uri.t Schema.schema
end

include module type of Schemas_intf

val bool_diff : bool -> bool -> bool option

val int_diff : int -> int -> int option

val list_diff :
  ('v -> 'v -> 'p option) -> 'v list -> 'v list -> 'p option list option

val string_diff : string -> string -> string option

val unit_diff : unit -> unit -> unit option

val bool_patch : bool -> bool -> bool

val int_patch : int -> int -> int

val list_patch : ('p -> 'v -> 'v) -> 'p option list -> 'v list -> 'v list

val string_patch : string -> string -> string

val unit_patch : unit -> unit -> unit

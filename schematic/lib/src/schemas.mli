type any = Ezjsonm.value

type ignore = unit

val pp_ignore : Base.Formatter.t -> ignore -> unit

val equal_ignore : ignore -> ignore -> bool

val any_schema : any Schema.t

val equal_any : any -> any -> bool

val bool_schema : bool Schema.t

val float_schema : float Schema.t

val ignore_schema : ignore Schema.t

val int64_schema : int64 Schema.t

val int_schema : int Schema.t

val list_schema : 'a Schema.t -> 'a list Schema.t

val mapping_schema : 'a Schema.t -> (string * 'a) list Schema.t

val option_schema : 'a Schema.t -> 'a option Schema.t

val string_const_schema : string -> unit Schema.t

val string_schema : string Schema.t

val unit_schema : unit Schema.t

module Either : sig
  include module type of Base.Either

  val schema : 'a Schema.t -> 'b Schema.t -> ('a, 'b) t Schema.t
end

module Result : sig
  include module type of Base.Result

  val schema : 't Schema.t -> 'error Schema.t -> ('t, 'error) t Schema.t
end

module Ptime : sig
  include module type of Ptime with type t = Ptime.t and type span = Ptime.span

  val schema : Ptime.t Schema.t

  (** Unfortunately [Ptime] does not define a standard type for time points with
      timezone and uses tuples all around. Define ours.*)
  type local = Ptime.t * Ptime.tz_offset_s

  val local_schema : local Schema.t

  val date_schema : date Schema.t

  val span_schema : span Schema.t
end

module Uri : sig
  include module type of Uri with type t = Uri.t

  val schema : Uri.t Schema.t
end

open Base
module Schema : module type of Schema

type json = Ezjsonm.value
type ignore = unit

val array_schema : 'a Schema.t -> 'a array Schema.t
val array_schema_versioned : Schema.version -> 'a Schema.t -> 'a array Schema.t
val pp_ignore : Base.Formatter.t -> ignore -> unit
val equal_ignore : ignore -> ignore -> bool
val compare_ignore : ignore -> ignore -> int
val bool_schema : bool Schema.t
val bool_schema_versioned : Schema.version -> bool Schema.t
val char_schema : char Schema.t
val char_schema_versioned : Schema.version -> char Schema.t
val float_schema : float Schema.t
val float_schema_versioned : Schema.version -> float Schema.t
val ignore_schema : ignore Schema.t
val ignore_schema_versioned : Schema.version -> ignore Schema.t
val int64_schema : int64 Schema.t
val int64_schema_versioned : Schema.version -> int64 Schema.t
val int_schema : int Schema.t
val int_schema_versioned : Schema.version -> int Schema.t
val json_schema : json Schema.t
val json_schema_versioned : Schema.version -> json Schema.t
val equal_json : json -> json -> bool
val list_schema : 'a Schema.t -> 'a list Schema.t
val list_schema_versioned : Schema.version -> 'a Schema.t -> 'a list Schema.t
val mapping_schema : 'a Schema.t -> (string * 'a) list Schema.t
val option_schema : 'a Schema.t -> 'a option Schema.t

val option_schema_versioned :
  Schema.version -> 'a Schema.t -> 'a option Schema.t

val string_const_schema : string -> unit Schema.t
val string_schema : string Schema.t
val string_schema_versioned : Schema.version -> string Schema.t
val unit_schema : unit Schema.t
val unit_schema_versioned : Schema.version -> unit Schema.t

module Bool : sig
  include module type of Bool

  val schema : t Schema.t
  val schema_versioned : Schema.version -> t Schema.t
end

module Char : sig
  include module type of Char

  val schema : t Schema.t
  val schema_versioned : Schema.version -> t Schema.t
end

module Either : sig
  include module type of Base.Either

  val schema : 'a Schema.t -> 'b Schema.t -> ('a, 'b) t Schema.t

  val schema_versioned :
    Schema.version -> 'a Schema.t -> 'b Schema.t -> ('a, 'b) t Schema.t
end

module Int : sig
  include module type of Int

  val schema : t Schema.t
  val schema_versioned : Schema.version -> t Schema.t
end

module Result : sig
  include module type of Base.Result

  val schema : 't Schema.t -> 'error Schema.t -> ('t, 'error) t Schema.t

  val schema_versioned :
    Schema.version -> 't Schema.t -> 'error Schema.t -> ('t, 'error) t Schema.t
end

module Ptime : sig
  include module type of Ptime with type t = Ptime.t and type span = Ptime.span

  type t = Ptime.t

  val schema : Ptime.t Schema.t
  val schema_versioned : Schema.version -> Ptime.t Schema.t

  (** Unfortunately [Ptime] does not define a standard type for time points with
      timezone and uses tuples all around. Define ours.*)
  type local = Ptime.t * Ptime.tz_offset_s

  val equal_local : local -> local -> bool
  val pp_local : local Fmt.t
  val local_schema : local Schema.t
  val local_schema_versioned : Schema.version -> local Schema.t
  val equal_date : date -> date -> bool
  val date_schema : date Schema.t
  val date_schema_versioned : Schema.version -> date Schema.t
  val span_schema : span Schema.t
  val span_schema_versioned : Schema.version -> span Schema.t
end

module String : sig
  include module type of String

  val schema : t Schema.t
  val schema_versioned : Schema.version -> t Schema.t
end

module Uri : sig
  include module type of Uri with type t = Uri.t

  val schema : Uri.t Schema.t
  val schema_versioned : Schema.version -> Uri.t Schema.t
end

val one_or_many_versioned : 't Schema.t -> Schema.version -> 't list Schema.t
val one_or_many : 't Schema.t -> 't list Schema.t

module Pair : sig
  type ('a, 'b) t = 'a * 'b

  val schema : 'a Schema.t -> 'b Schema.t -> ('a, 'b) t Schema.t

  val schema_versioned :
    Schema.version -> 'a Schema.t -> 'b Schema.t -> ('a, 'b) t Schema.t
end

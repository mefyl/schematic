open Batteries

include module type of Json_intf

open Schema

type t

val decode : 'a schema -> t -> 'a Result.t

val encode : 'a schema -> 'a -> t

val from_channel : in_channel -> (t, [> parse_error ]) Batteries.Result.t

val to_channel : out_channel -> t -> unit

val of_string : string -> (t, [> parse_error ]) Batteries.Result.t

val to_string : t -> string

val pp : Format.formatter -> t -> unit

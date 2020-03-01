open Schema

type t

val decode : 'a schema -> t -> 'a

val encode : 'a schema -> 'a -> t

val from_channel : in_channel -> t

val to_channel : out_channel -> t -> unit

val of_string : string -> t

val to_string : t -> string

val of_schema : 'a schema -> t

val pp : Format.formatter -> t -> unit

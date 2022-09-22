(* Hide concrete type to prevent misuse *)
type t

val make : string -> t
val map : f:(string -> string) -> t -> t
val to_string : t -> string
val pp : t Fmt.t
val equal : t -> t -> bool
val compare : t -> t -> int

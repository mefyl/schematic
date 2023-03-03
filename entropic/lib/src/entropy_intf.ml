module type S = sig
  type t

  val get : t -> int -> t * Bytes.t option
end

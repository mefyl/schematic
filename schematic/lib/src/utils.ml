module E = Error
include Base
module Error = E

module type Monad = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module Let_syntax (Monad : Monad) = struct
  let ( >>= ) = Monad.bind
end

let escape s =
  let open Fmt in
  const Dump.string s

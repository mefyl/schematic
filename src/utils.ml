module type Monad = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t
end

module Let_syntax (Monad : Monad) = struct
  let ( >>= ) = Monad.bind
end

let escape str =
  let open Pp in
  let open Pp.O in
  text "\"" ++ verbatim (String.escaped str) ++ text "\""

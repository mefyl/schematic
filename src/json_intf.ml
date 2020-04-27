open Batteries

module Result = struct
  (* FIXME: 4.06: include Result instead *)

  type 'a t = ('a, [ `Json_decode of string ]) Result.t

  let ok = Result.ok

  let error = Result.error

  let bind = Result.bind

  let return = Result.ok

  let both l r =
    match (l, r) with
    | Result.Ok l, Result.Ok r -> Result.Ok (l, r)
    | Result.Error e, _ -> Result.Error e
    | _, Result.Error e -> Result.Error e
end

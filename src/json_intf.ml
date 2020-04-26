open Batteries

module Result = struct
  include Result

  type 'a t = ('a, [ `Json_decode of string ]) Result.t

  let return = Result.ok

  let both l r =
    match (l, r) with
    | Result.Ok l, Result.Ok r -> Result.Ok (l, r)
    | Result.Error e, _ -> Result.Error e
    | _, Result.Error e -> Result.Error e
end

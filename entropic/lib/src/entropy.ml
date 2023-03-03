open Base
include Entropy_intf

module Bytes = struct
  type t = {
    data : Bytes.t;
    pos : int;
  }

  let make data = { data; pos = 0 }

  let get ({ data; pos } as t) n =
    let new_pos = pos + n in
    if new_pos > Bytes.length data then (t, None)
    else ({ t with pos = new_pos }, Some (Bytes.sub data ~pos ~len:n))
end

module Entropy = struct
  type t = Entropy : (module S with type t = 't) * 't -> t

  let get (Entropy (((module E) as impl), t)) n =
    let t, res = E.get t n in
    (Entropy (impl, t), res)

  let make impl t = Entropy (impl, t)
end

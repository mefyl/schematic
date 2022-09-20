open Base

(** A Ptime.t is assumed to have an int * int64 layout. *)
let ptime =
  lazy
    ((* This value has nothing special about it. *)
     let ref_val = (5432, 34323443L) in
     let ptime_repr = Stdlib.Obj.repr ref_val in
     let assumed_repr =
       Stdlib.Obj.repr (Schemas.Ptime.unsafe_of_d_ps ref_val)
     in
     assert (Poly.(ptime_repr = assumed_repr)))

let ptime_local =
  lazy
    (* This one doesn't require doing at runtime. We're asserting type equality
       at compilation time. *)
    (let open Schemas in
    let x : Ptime.t * int = (Ptime.min, 1) in
    let (_ : Ptime.local) = x in
    ())

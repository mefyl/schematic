open Base

(* Runtime checks *)

(** A Ptime.t is assumed to have an int * int64 layout. *)
let ptime =
  lazy
    ((* This value has nothing special about it. *)
     let ref_val = (5432, 34323443L) in
     let ptime_repr = Stdlib.Obj.repr ref_val in
     let assumed_repr =
       Stdlib.Obj.repr (Schematic.Schemas.Ptime.unsafe_of_d_ps ref_val)
     in
     assert (Poly.(ptime_repr = assumed_repr)))

(* Compile-time type assertions - we technically don't need lazy values at all
   here, but this is in case those end up incorporating runtime checks as
   well. *)

let ptime_local =
  lazy
    (let open Schematic.Schemas in
    let x : Ptime.t * int = (Ptime.min, 1) in
    let (_ : Ptime.local) = x in
    ())

let date =
  lazy
    (let open Schematic.Schemas in
    let x : int * int * int = (2022, 7, 1) in
    let (_ : Ptime.date) = x in
    ())

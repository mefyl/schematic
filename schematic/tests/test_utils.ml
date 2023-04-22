open Base

module String_pair = struct
  module T = struct
    type t = string * string [@@deriving ord, show]

    let sexp_of_t (l, r) = Sexp.(List [ Atom l; Atom r ])
  end

  include T
  include Comparable.Make (T)

  type elem = t
end

module Testables = struct
  module Set = struct
    (** Functor *)

    module type SET = sig
      type elem
      type comparator_witness

      val pp : elem Fmt.t
      val comparator : (elem, comparator_witness) Comparator.t
    end

    module Make (S : SET) : sig
      type elem = S.elem
      type set = (S.elem, S.comparator_witness) Base.Set.t

      val comparator : (S.elem, S.comparator_witness) Comparator.t
      val set : set Alcotest.testable
    end = struct
      let comparator = S.comparator

      type elem = S.elem
      type set = (S.elem, S.comparator_witness) Base.Set.t

      let set =
        let pp : set Fmt.t =
         fun f s ->
          let l = s |> Base.Set.to_list in
          (Fmt.list S.pp ~sep:Fmt.semi) f l
        in
        let eq : set -> set -> bool = Base.Set.equal in
        Alcotest.testable pp eq
    end

    (* Functor applications *)

    module String = Make (struct
      include String

      type elem = t
    end)

    module String_pair = Make (String_pair)
  end
end

module Tests = Tests

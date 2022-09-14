open Base

module type MARK = sig
  type t
end

module Make (S : MARK) = struct
  module rec Mark :
    (Comparable.S with type t = string * (S.t * Marks.t) Lazy.t) = struct
    module T = struct
      type t = string * (S.t * Marks.t) Lazy.t

      let sexp_of_t (id, _) = Sexp.Atom id
      let compare (l, _) (r, _) = String.compare l r
    end

    include T
    include Base.Comparable.Make (T)
  end

  and Marks : sig
    type t

    val empty : t
    val make : string -> (unit -> S.t * Marks.t) -> t
    val add : t -> Mark.t -> t
    val fold : t -> init:'a -> f:('a -> Mark.t -> 'a) -> 'a
    val has : t -> Mark.t -> bool
    val merge : t list -> t
    val of_list : Mark.t list -> t
  end = struct
    type t = (Mark.t, Mark.comparator_witness) Set.t

    let empty = Set.empty (module Mark)
    let make id f = Set.of_list (module Mark) [ (id, lazy (f ())) ]
    let add = Set.add
    let fold = Set.fold
    let has = Set.mem
    let merge = Set.union_list (module Mark)
    let of_list = Set.of_list (module Mark)
  end
end

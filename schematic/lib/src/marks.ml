open Base

module type MARK = sig
  type t

  val pp : t Fmt.t

  module Id : sig
    type t

    val make : string -> t
  end
end

module type FOO = sig
  type u

  include Comparable.S with type t = u

  val pp : ?with_value:bool -> t Fmt.t
end

module Make (S : MARK) = struct
  module rec Mark : (FOO with type u = string * (S.t * Marks.t) Lazy.t) = struct
    module T = struct
      type t = string * (S.t * Marks.t) Lazy.t
      type u = t

      let sexp_of_t (id, _) = Sexp.Atom id
      let compare (l, _) (r, _) = String.compare l r
    end

    include T
    include Base.Comparable.Make (T)

    let pp : ?with_value:bool -> t Fmt.t =
     fun ?(with_value = true) f (s, v) ->
      if with_value then
        let open Fmt in
        let inner : (_ * (_ * _)) Fmt.t =
         fun f (s, (v, _r)) ->
          (const string s ++ const char ',' ++ sp ++ const char '{'
         ++ const S.pp v ++ const char ','
         (* XXX: cannot use Marks.pp here (unsafe module recursiveness) *)
         ++ const string " ..."
          ++ const char '}')
            f ()
        in
        (const char '(' ++ inner ++ const char ')') f (s, Lazy.force v)
      else Fmt.string f s
  end

  and Marks : sig
    type t

    val empty : t
    val make : string -> (unit -> S.t * Marks.t) -> t
    val add : t -> Mark.t -> t
    val map : t -> f:(Mark.t -> Mark.t) -> t
    val fold : t -> init:'a -> f:('a -> Mark.t -> 'a) -> 'a
    val has : t -> Mark.t -> bool
    val merge : t list -> t
    val of_list : Mark.t list -> t
    val pp : t Fmt.t
  end = struct
    type t = (Mark.t, Mark.comparator_witness) Set.t

    let empty = Set.empty (module Mark)
    let make id f = Set.of_list (module Mark) [ (id, lazy (f ())) ]
    let add = Set.add
    let map t = Set.map (module Mark) t
    let fold = Set.fold
    let has = Set.mem
    let merge = Set.union_list (module Mark)
    let of_list = Set.of_list (module Mark)

    let pp : t Fmt.t =
     fun f t ->
      let l = Set.to_list t in
      let open Fmt in
      let sep = const char ',' ++ sp in
      let pp =
        const string "Marks:" ++ sp ++ const char '(' ++ list Mark.pp ~sep
        ++ const char ')'
      in
      pp f l
  end

  let rec of_mark marks mark =
    if Marks.has marks mark then ([], marks)
    else
      let res, deps =
        let res, deps = Lazy.force @@ snd mark in
        ([ (S.Id.make @@ fst mark, res) ], deps)
      in
      let marks = Marks.add marks mark in
      let f (res, marks) mark =
        let deps, marks = of_mark marks mark in
        (res @ deps, marks)
      in
      Marks.fold ~init:(res, marks) ~f deps
end

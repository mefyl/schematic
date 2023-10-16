open Base

module type MARK = sig
  type t

  val pp : t Fmt.t

  module Id : sig
    type t

    val make : string -> t
  end
end

module type V = sig
  type t

  include Comparable.S with type t := t

  val pp : ?with_value:bool -> t Fmt.t
end

module Make (S : MARK) = struct
  (** A [Mark.t] is a named value, which may depend on other named values. *)
  module rec Mark : (V with type t = string * (S.t * Marks.t) Lazy.t) = struct
    module T = struct
      type t = string * (S.t * Marks.t) Lazy.t

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
      else Fmt.(string) f s
  end

  (** [Marks.t] is a set of named values. *)
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
    (* State *)
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

  (** [of_marks marks mark] returns a tuple:

      - The list of newly-emitted values derived from Mark.t
      - The merged state corresponding to the addition of [mark] to [marks] *)
  let rec of_mark (marks : Marks.t) (mark : Mark.t) :
      (S.Id.t * S.t) list * Marks.t =
    if Marks.has marks mark then ([], marks)
    else
      let leaf, deps =
        let res, deps = Lazy.force @@ snd mark in
        ([ (S.Id.make (fst mark), res) ], deps)
      in
      let marks = Marks.add marks mark in
      let f (res, marks) mark =
        let deps, marks = of_mark marks mark in
        (res @ deps, marks)
      in
      let l, marks = Marks.fold ~init:([], marks) ~f deps in
      (l @ leaf, marks)

  module Make_mark_converter (M : sig
    val identifier : 'a Schema_intf.t -> S.t -> string

    val convert :
      string option ->
      'a Schema_intf.descriptor ->
      'a Schema_intf.parametric option ->
      S.t * Marks.t
  end) =
  struct
    let of_descriptor d = M.convert None d None

    let mark_of_schema (Schema.{ id; descriptor; parametric } as schema) =
      let ((t, _) as res) = M.convert id descriptor parametric in
      let id = M.identifier schema t in
      (id, lazy res)

    let of_schema_state marks s = of_mark marks @@ mark_of_schema s

    let unpacker marks =
      let unpack : type a. a Schema.t -> (S.Id.t * S.t) list * Marks.t =
       fun s -> of_schema_state marks s
      in
      { Schema.unpack }

    let of_schemas_state marks schemas =
      let f (res, marks) s =
        let ts, marks = Schema.unpack (unpacker marks) s in
        (res @ ts, marks)
      in
      List.fold_left ~init:([], marks) ~f schemas

    type state = Marks.t

    let state_empty = Marks.empty

    let state_exclude schemas =
      Marks.of_list
        (List.map ~f:(Schema.unpack { unpack = mark_of_schema }) schemas)

    let of_schemas ?(exclude = []) schemas =
      let marks = state_exclude exclude in
      of_schemas_state marks schemas |> fst

    let of_schema schema = of_schema_state Marks.empty schema |> fst
  end
end

open Base
open Schematic

module type MARK = sig
  type t

  val pp : t Fmt.t
  val compare : t -> t -> int
  val prevent_conflicts : bool

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

  (** [Marks.t] is a set of named values. There may never be two values sharing
      a name but with different internal structures. *)
  and Marks : sig
    type t

    val empty : t
    val make : string -> (unit -> S.t * Marks.t) -> t
    val add : t -> Mark.t -> t
    val fold : t -> init:'a -> f:('a -> Mark.t -> 'a) -> 'a
    val has : t -> Mark.t -> bool
    val merge : t list -> t
    val of_list : Mark.t list -> t
    val pp : t Fmt.t
  end = struct
    type t = (string, (S.t * Marks.t) Lazy.t, String.comparator_witness) Map.t

    let empty = Map.empty (module String)
    let make id f = Map.add_exn empty ~key:id ~data:(Lazy.from_fun f)

    let add (t : t) ((id, x) : Mark.t) =
      match Map.add t ~key:id ~data:x with `Ok t -> t | `Duplicate -> t

    let fold (t : t) ~init ~f =
      Map.fold ~init
        ~f:(fun ~key ~data acc ->
          let x : Mark.t = (key, data) in
          f acc x)
        t

    let has (t : t) ((id, x) : Mark.t) = Map.mem t id

    let merge l =
      List.fold l ~init:(empty) ~f:(fun acc x ->
          Map.merge_skewed acc x ~combine:(fun ~key l r -> l (* XXX *))
        )

    let of_list (l : Mark.t list) =
      Map.of_alist_reduce (module String) l  ~f:(fun l r -> l (* XXX *))

    let pp : t Fmt.t =
     fun f t ->
      let l = Map.to_alist t in
      let open Fmt in
      let sep = const char ',' ++ sp in
      let pp =
        const string "Marks:" ++ sp ++ const char '(' ++ list Mark.pp ~sep
        ++ const char ')'
      in
      pp f l
  end

  (** [of_marks marks mark] *)
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
    val identified : S.t -> string option * S.t
    val of_descriptor : string option -> 'a Schema.descriptor -> S.t * Marks.t
    val of_parametric : 'a Schema.parametric -> S.t * Marks.t
  end) =
  struct
    let of_descriptor = M.of_descriptor
    let of_parametric = M.of_parametric

    let mark_of_schema Schema.{ id; descriptor; parametric } =
      let res, marks = M.of_descriptor id descriptor in
      let id_inner, res = M.identified res in
      let id = Option.first_some id id_inner |> Option.value_exn in
      match parametric with
      | None -> (id, lazy (res, marks))
      | Some parametric -> (id, lazy (M.of_parametric parametric))

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

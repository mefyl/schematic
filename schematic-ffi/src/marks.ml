open Base
open Schematic

module type MARK = sig
  type t

  val pp : t Fmt.t
  val equal : t -> t -> bool

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
  let ensure_consistent id l r =
    if not (S.equal (fst l) (fst r)) then
      Fmt.failwith "'%s' had duplicate definitions" id
    else l

  module rec Mark : (V with type t = string * (S.t * Marks.t)) = struct
    module T = struct
      type t = string * (S.t * Marks.t)

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
        (const char '(' ++ inner ++ const char ')') f (s, v)
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
    val find : t -> string -> (S.t * Marks.t) option
    val merge : t list -> t
    val of_list : Mark.t list -> t
    val pp : t Fmt.t
  end = struct
    type t = (string, S.t * Marks.t, String.comparator_witness) Map.t

    let empty = Map.empty (module String)

    let make id f =
      (* We don't actually use laziness at this point; we don't support
         recursive data types, and we want safety checks. *)
      let data = f () in
      Map.add_exn empty ~key:id ~data

    let add (t : t) ((id, x) : Mark.t) =
      Map.change t id ~f:(function
        | None -> Some x
        | Some existing -> Some (ensure_consistent id existing x))

    let fold (t : t) ~init ~f =
      Map.fold ~init
        ~f:(fun ~key ~data acc ->
          let x : Mark.t = (key, data) in
          f acc x)
        t

    let find (t : t) id = Map.find t id

    let merge (l : t list) =
      List.fold l ~init:empty ~f:(fun acc x ->
          Map.merge_skewed acc x ~combine:(fun ~key l r ->
              ensure_consistent key l r))

    let of_mark ((id, x) : Mark.t) = make id (fun () -> x)

    let of_list (l : Mark.t list) =
      (* Very inefficient *)
      l |> List.map ~f:of_mark |> merge

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
    let id = fst mark in
    match Marks.find marks id with
    | Some existing ->
      let _ = ensure_consistent id existing (snd mark) in
      ([], marks)
    | None ->
      let leaf, deps =
        let res, deps = snd mark in
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
      | None -> (id, (res, marks))
      | Some parametric -> (id, M.of_parametric parametric)

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

open Base
include C_intf

(* XXX: legacy, need to review & test identifier conversion *)
let normalize_id s = String.split ~on:'-' s |> String.concat ~sep:"_"

include Marks.Make (struct
  type t = c

  (* potential FIXME (check valid identifiers in C) *)
  let normalize_id = normalize_id
end)

module Of = C_walk.Walk (struct
  type res = t * Marks.t
  type fields = (string * t) list * Marks.t
  type case = string * t
  type t = res

  let make name = (Atom (name, None), Marks.empty)

  (* Shallow primitive types *)

  (* unit descriptors *)
  let ignore () = make "unit"
  let null = ignore
  let string_const _s = (Void, Marks.empty)

  (* Primitive types without indirection *)
  let int () = make "int"
  let bool () = make "bool"

  (** "OCaml's floating-point numbers follow the IEEE 754 standard, using double
      precision (64 bits) numbers." *)
  let float () = make "double"

  let int64 () = make "int64_t"
  let string () = (String, Marks.empty)

  (* Some careful considerations to make here:

     - Recursive data will most definitely need to use pointers.

     - If we try to force the schema here and there is a cycle, we'll enter an
     infinite loop. *)
  let schema (id : string option) parametric s =
    Fmt.epr "id: %a\n@." Fmt.(option string) id;
    match parametric with
    | Some parametric -> parametric ()
    | None -> (
      match id with
      | None ->
        Fmt.epr "no id\n";
        let c, marks = s () in
        Fmt.epr "c: %a\n" pp c;
        (c, marks)
      | Some id ->
        Fmt.epr "has id: %s\n" id;
        let referenced_type : c = fst @@ s () in
        (Atom (id, Some referenced_type), Marks.make id s))

  let fields_empty () = ([], Marks.empty)

  let field (name : string) (_required : bool) ((ast, marks) : t)
      ((rest_ast, rest_marks) : fields) =
    let marks = Marks.merge [ marks; rest_marks ] in
    ((name, ast) :: rest_ast, marks)

  let object_ ((fields_cases, marks) : fields) =
    (Struct { fields = fields_cases }, marks)

  let union ~name ~key:_ cases =
    let constructors, marks =
      cases |> List.map ~f:(fun (s, (c, m)) -> ((s, c), m)) |> List.unzip
    in
    let marks = marks |> Marks.merge in

    let basic, rich =
      constructors
      |> List.partition_map ~f:(function
           | s, Void -> First s
           | (_, _) as x -> Second x)
    in

    (Variant { name = Option.value_exn name; basic; rich }, marks)

  let tuple ts =
    let items, marks = List.unzip ts in
    let tuptyp = Tuple items in
    let s () = (tuptyp, Marks.merge marks) in
    let tuple_name = tuple_name items in
    (Atom (tuple_name, Some tuptyp), Marks.make tuple_name s)

  let list (c, marks) =
    let arrtyp = Array c in
    let s () = (arrtyp, marks) in
    let name = array_name c in
    (Atom (name, Some arrtyp), Marks.make name s)

  (* This could be optimized (null ptr optim), but this approach allows us to
     simplify things *)
  let nullable (c, marks) =
    let name = c_typename c ^ "_option" in
    (Variant { name; basic = [ "none" ]; rich = [ ("some", c) ] }, marks)

  (* Simplest possible encoding *)
  (* We could use a struct for "key" / "value" fields instead of a tuple. *)
  let mapping t = tuple [ string (); t ] |> list

  let time () =
    Lazy.force C_sanity_checks.ptime;
    make "timespec"

  (* TODO: for clarity, use a struct *)
  let time_local () =
    Lazy.force C_sanity_checks.ptime_local;
    tuple [ time (); int () ]

  let any () = (Atom ("json", Some String), Marks.empty)

  let date () =
    Lazy.force C_sanity_checks.date;
    (* This will work because a Ptime.date is a (int * int * int), which has the
       same memory representation as a struct with three int fields. *)
    let int = fst (int ()) in
    let fields = [ ("year", int); ("month", int); ("day", int) ] in
    (Struct { fields }, Marks.empty)

  (* Unimplemented *)

  (* FIXME: any parametric type gets mapped into `unit` for now. *)
  let parametric template_name _args _effective _t =
    Fmt.epr "tn: %s@." template_name;
    ignore ()
end)

(* XXX: copied from TypeScript, might be generalizable? *)
let mark_of_schema Schema.{ id; descriptor; parametric } =
  match parametric with
  | None ->
    (Option.value_exn ~here:[%here] id, lazy (Of.descriptor id descriptor))
  | Some parametric ->
    (Option.value_exn ~here:[%here] id, lazy (Of.parametric parametric))

let of_schema marks s = of_mark marks @@ mark_of_schema s

let unpacker marks =
  let unpack : type a. a Schema.t -> (string * t) list * Marks.t =
   fun s -> of_schema marks s
  in
  { Schema.unpack }

let of_schemas_state marks schemas =
  let f (res, marks) s =
    let ts, marks = Schema.unpack (unpacker marks) s in
    (ts @ res, marks)
  in
  List.fold_left ~init:([], marks) ~f schemas

let of_schema schema = of_schema Marks.empty schema |> fst

type state = Marks.t

let state_empty = Marks.empty

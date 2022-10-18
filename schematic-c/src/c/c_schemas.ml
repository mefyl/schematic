open Base
include C_intf

(* Shamelessly copy-pasted, because depending on either ppx_deriving.api or
   ocaml-compiler-libs.common confuses esy .*)
let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := (223 * !accu) + Stdlib.Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land ((1 lsl 31) - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

include Schematic.Marks.Make (struct
  type t = c

  let pp = pp_c

  module Id = Identifier
end)

module Of = C_walk.Walk (struct
  type res = c * Marks.t
  type fields = (string * c) list * Marks.t
  type case = string * c
  type t = res

  let name (c, _marks) = c_typename c |> Identifier.to_string
  let make name = (Atom (Identifier.make name, None), Marks.empty)

  (* Shallow primitive types *)

  let ignore () = make "unit"
  let null = ignore

  (* Mainly used for individual union constructors *)
  let string_const _s = (Void, Marks.empty)
  let int () = make "int"
  let bool () = make "bool"

  (** "OCaml's floating-point numbers follow the IEEE 754 standard, using double
      precision (64 bits) numbers." *)
  let float () = make "double"

  let int64 () = make "int64_t"
  let string () = (String, Marks.empty)

  (* This will recurse forever if given a recursive schema. *)
  let schema (id : string option) parametric s =
    Logs.debug (fun m -> m "id: %a\n@." Fmt.(option string) id);
    match parametric with
    | Some parametric -> parametric ()
    | None -> (
      match id with
      | None ->
        Logs.debug (fun m -> m "no id\n");
        let c, marks = s () in
        Logs.debug (fun m -> m "c: %a\n" pp_c c);
        (c, marks)
      | Some id ->
        Logs.debug (fun m -> m "has id: %s\n" id);
        let referenced_type : c = fst @@ s () in
        (Atom (Identifier.make id, Some referenced_type), Marks.make id s))

  (* This could be optimized (null ptr optim), but this approach allows us to
     simplify things

     TODO: better representation, as we don't need different tags for each
     option specialization. *)
  let nullable (c, marks) =
    let name = c_typename c |> Identifier.map ~f:(fun s -> s ^ "_option") in
    let s () =
      ( Variant
          {
            name;
            basic = [ Identifier.make "none" ];
            rich = [ (Identifier.make "some", c) ];
          },
        marks )
    in
    schema (Some (Identifier.to_string name)) None s

  let fields_empty () = ([], Marks.empty)

  let field : string -> required:bool -> t -> fields -> fields =
   fun name ~required (ast, marks) (rest_ast, rest_marks) ->
    let transform = if required then Fn.id else nullable in
    let ast, marks = transform (ast, marks) in
    let marks = Marks.merge [ marks; rest_marks ] in
    ((name, ast) :: rest_ast, marks)

  let object_ ((fields, marks) : fields) =
    if List.is_empty fields then (Void, marks)
    else
      let fields =
        fields |> List.map ~f:(fun (s, c) -> (Identifier.make s, c))
      in
      (Struct { fields }, marks)

  let union ~name ~polymorphic ~(cases : (string * string * (c * Marks.t)) list)
      =
    let name = Option.value_exn name in
    let constructors, marks =
      cases
      |> List.map ~f:(fun (s, raw_s, (c, m)) ->
             ((Identifier.make s, raw_s, c), m))
      |> List.unzip
    in
    let marks = marks |> Marks.merge in
    let basic, rich =
      constructors
      |> List.partition_map ~f:(function
           | s, raw_s, Void -> First (s, raw_s)
           | x -> Second x)
    in
    (* Give names to inline payload types that would otherwise be impossible to
       refer to from Swift. We do this by creating extra marks.

       XXX: would be nice to reference those marks in the converter, but as it
       stands, this might break the distinction between inline and regular
       payloads *)
    let inline_payload_marks =
      rich
      |> List.map ~f:(fun (id, _, constr_c) ->
             match constr_c with
             | Tuple _ | Struct _ ->
               let id =
                 inline_record_c_name ~variant_name:name ~constructor_name:id
               in
               let s () = (constr_c, marks) in
               Marks.make id s
             | _ -> Marks.empty)
      |> Marks.merge
    in
    let marks = Marks.merge [ marks; inline_payload_marks ] in
    let name = Identifier.make name in
    if polymorphic then
      let basic =
        List.map ~f:(fun (id, raw_name) -> (id, hash_variant raw_name)) basic
      in
      let rich =
        List.map
          ~f:(fun (id, raw_name, c) -> (id, hash_variant raw_name, c))
          rich
      in
      (Poly_variant { name; basic; rich }, marks)
    else
      let basic = List.map ~f:(fun (id, _) -> id) basic in
      let rich = List.map ~f:(fun (id, _, c) -> (id, c)) rich in
      (Variant { name; basic; rich }, marks)

  let tuple ts =
    let items, marks = List.unzip ts in
    let s () = (Tuple items, Marks.merge marks) in
    let tuple_name = tuple_name items in
    schema (Some (Identifier.to_string tuple_name)) None s

  let list (c, marks) =
    let s () = (Array c, marks) in
    let name = array_name c in
    schema (Some (Identifier.to_string name)) None s

  (* Simplest possible encoding *)
  (* We could use a struct for "key" / "value" fields instead of a tuple. *)
  let mapping t = tuple [ string (); t ] |> list

  let time () =
    Lazy.force C_sanity_checks.ptime;
    make "sch_time"

  (* TODO: for clarity, use a struct *)
  let time_local () =
    Lazy.force C_sanity_checks.ptime_local;
    tuple [ time (); int () ]

  let any () = (Atom (Identifier.make "sch_json", Some String), Marks.empty)

  let date () =
    Lazy.force C_sanity_checks.date;

    let s () =
      let int = fst (int ()) in
      (* This will work because a Ptime.date is a (int * int * int), which has
         the same memory representation as a struct with three int fields. *)
      let fields =
        [
          (Identifier.make "year", int);
          (Identifier.make "month", int);
          (Identifier.make "day", int);
        ]
      in
      (Struct { fields }, Marks.empty)
    in
    schema (Some "sch_date") None s

  let mapped : string option -> t -> t =
   fun id (c, marks) ->
    let front_facing, marks =
      let s () = (c, marks) in
      let default = c_typename c |> Identifier.to_string in
      let id = Fmt.str "%s_external" (Option.value id ~default) in
      schema (Some id) None s
    in
    (Mapped { front_facing }, marks)
end)

include Make_mark_converter (struct
  let of_descriptor = Of.descriptor
  let of_parametric = Of.parametric
end)

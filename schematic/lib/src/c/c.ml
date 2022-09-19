open Base
include C_intf

type c = t

let normalize_id s = s

include Marks.Make (struct
  type t = c

  (* potential FIXME (check valid identifiers in C) *)
  let normalize_id = normalize_id
end)

module Of = Cwalk.Walk (struct
  type res = t * Marks.t
  type fields = (string * t) list * Marks.t
  type case = string * t
  type t = res

  let make name = (Atom name, Marks.empty)

  (* Shallow primitive types *)

  (* Zero-Sized Types *)
  let ignore _ = (Void, Marks.empty)
  let null = ignore
  let string_const = ignore

  (* Primitive types without indirection *)
  let int () = make "int"
  let bool () = make "bool"
  let float () = make "float"

  (* XXX - Might need special handling on the caml side *)
  let int64 () = make "int64_t"

  (* XXX *)
  let string () = (Pointer (Atom "char"), Marks.empty)

  (* Some careful considerations to make here:

     - Recursive data will most definitely need to use pointers. - If we try to
     force the schema here and there is a cycle, we'll enter an infinite
     loop. *)
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
        (Atom (normalize_id id), Marks.make id s))

  let fields_empty () = ([], Marks.empty)

  let field (name : string) (_required : bool) ((ast, marks) : t)
      ((rest_ast, rest_marks) : fields) =
    let marks = Marks.merge [ marks; rest_marks ] in

    let ast =
      match ast with
      | (Atom "int" | Atom "char") as prim -> prim
      (* XXX *)
      | Atom s when String.is_prefix ~prefix:"tuple_" s -> Atom s
      | Atom _ as nonprim -> Pointer nonprim
      | Pointer (Atom _) as p -> p
      | Tuple _ as tup -> tup
      | _ as x -> failwith (Fmt.str "nah: %a" pp x)
    in
    ((name, ast) :: rest_ast, marks)

  let object_ ((fields_cases, marks) : fields) =
    (Struct { fields = fields_cases }, marks)

  let union ~name ~key:_ cases =
    let constructors, marks =
      cases |> List.map ~f:(fun (s, (c, m)) -> ((s, c), m)) |> List.unzip
    in
    let marks = marks |> Marks.merge in
    (Variant (Option.value_exn name, constructors), marks)

  let tuple ts =
    let items, marks = List.unzip ts in
    (* FIXME *)
    let s () = (Tuple items, Marks.merge marks) in
    let tuple_name = tuple_name items in
    (Atom tuple_name, Marks.make tuple_name s)

  (* Unimplemented *)

  let parametric template_name _args _effective _t =
    Fmt.epr "tn: %s@." template_name;
    failwith "parametric: unimpl"

  let any () = failwith "unimpl: nay"
  let date _ = failwith "unimplemented: date "
  let mapping _ = failwith "unimplemented: mapping "
  let list _ = failwith "unimplemented: list "
  let nullable _ = failwith "unimplemented: nullable "
  let time _ = failwith "unimplemented: time "
  let time_local _ = failwith "unimplemented: time_local "
end)

(* XXX: copied from TypeScript, might be generalizable? *)
let mark_of_schema Schema.{ id; descriptor; parametric } =
  match parametric with
  | None ->
    (Option.value_exn ~here:[%here] id, lazy (Of.descriptor id descriptor))
  | Some _parametric -> failwith "no parametrics yet"

let of_schema marks s = of_mark marks @@ mark_of_schema s

let unpacker marks =
  let unpack : type a. a Schema.t -> (string * t) list * Marks.t =
   fun s -> of_schema marks s
  in
  { Schema.unpack }

let of_schemas_state marks schemas =
  let f (res, marks) s =
    let ts, marks = Schema.unpack (unpacker marks) s in
    (res @ ts, marks)
  in
  List.fold_left ~init:([], marks) ~f schemas

let of_schema schema = of_schema Marks.empty schema |> fst
(* End of shameless copy *)

(* XXX: Butchering the boxing logic while I learn it. *)

let pp ?(indent = 2) fmt ts =
  let rec pp fmt ts =
    let printer =
      let open Fmt in
      let braced lhs inner : _ Fmt.t =
        (vbox ~indent @@ (lhs ++ const string " {" ++ cut ++ inner))
        ++ cut ++ const char '}'
      in

      let struct_or_union_field f (name, v) =
        let open Fmt in
        let pp = box (Fmt.pair ~sep:sp (hvbox pp) Fmt.string) in
        pp f (v, name ^ ";")
      in

      let struct_ : (string * _) list Fmt.t =
        braced (const string "struct") (list ~sep:cut struct_or_union_field)
      in

      let union_ : (string * _) list Fmt.t =
        braced (const string "union") (list ~sep:cut struct_or_union_field)
      in

      let variant fmt (name, constructors) =
        let is_basic =
          List.for_all ~f:(function _, Void -> true | _ -> false) constructors
        in
        let basic, rich = basic_rich_split constructors in
        let variant_names = List.map ~f:fst basic @ List.map ~f:fst rich in

        (* An enum declaring each constructor of the variant. This is totally
           independent of whether there is a payload associated with those
           constructors. *)
        let enum =
          let tag_suffix = if is_basic then "" else "_tag" in
          braced (const string "enum")
            (const
               (list
                  ~sep:(const char ',' ++ cut)
                  (fun fmt s -> Fmt.fmt "%s_%s%s" fmt name s tag_suffix))
               variant_names)
        in

        let printer =
          if is_basic then enum
          else
            let tag_field = enum ++ const string " tag;" in
            let payload_field =
              const union_
                (constructors
                |> List.filter ~f:(function _, Void -> false | _ -> true))
              ++ const string " payload;"
            in
            let inner = tag_field ++ cut ++ payload_field in

            (* Kind of a special case of struct... *)
            braced (const string "struct") inner
        in
        printer fmt ()
      in

      match ts with
      | Atom s -> const string s
      | Pointer t -> const pp t ++ const char '*'
      | Struct { fields; _ } -> const struct_ fields
      (* Tuples are structs with types named according to their indices *)
      | Tuple types ->
        let fields = tuple_fields types in
        const struct_ fields
      | Variant v -> const variant v
      | Void -> nop
    in
    printer fmt ()
  in
  pp fmt ts

let to_c_type_expression = Fmt.to_to_string pp

include C_codegen

let to_of_caml_src name c = Fmt.to_to_string pp_of_caml_converter (name, c)
let to_to_caml_src name c = Fmt.to_to_string pp_to_caml_converter (name, c)

type state = Marks.t

let state_empty = Marks.empty

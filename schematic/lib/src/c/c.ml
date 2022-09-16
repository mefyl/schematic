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
  type t = res

  let make name = (Atom name, Marks.empty)

  (* Shallow primitive types *)
  let ignore () = make "void"

  (* XXX: bit width *)
  let int () = make "int"
  let string () = (Pointer (Atom "char"), Marks.empty)

  let schema (id : string option) parametric s =
    Fmt.epr "id: %a\n@." Fmt.(option string) id;
    match parametric with
    | Some parametric ->
      Fmt.epr "parametric\n";
      parametric ()
    | None -> (
      Fmt.epr "not parametric\n";
      match id with
      | None ->
        Fmt.epr "no id\n";
        s ()
      | Some id ->
        Fmt.epr "has id: %s\n" id;
        (Atom (normalize_id id), Marks.make id s))

  let parametric template_name _args _effective _t =
    Fmt.epr "tn: %s@." template_name;
    failwith "parametric: unimpl"

  let fields_empty () =
    Fmt.epr "fields_empty\n";
    ([], Marks.empty)

  let field_inline _required fields _rest =
    Fmt.epr "fields_inline\n";
    List.hd_exn fields

  let field_outline (name : string) (_required : bool) ((ast, marks) : t)
      ((rest_ast, rest_marks) : fields) =
    Fmt.epr "field_outline (name: %s)\n" name;
    Fmt.epr "field_outline 'ast': %s\n" (show ast);
    Fmt.epr "field_outline 'rest_ast': '%a'\n"
      Fmt.(list @@ pair string pp)
      rest_ast;
    Fmt.epr "\n";

    let marks = Marks.merge [ marks; rest_marks ] in

    let ast =
      match ast with
      | (Atom "int" | Atom "char") as prim -> prim
      | Atom _ as nonprim -> Pointer nonprim
      | Pointer (Atom _) as p -> p
      | _ as x -> failwith (Fmt.str "nah: %a" pp x)
    in

    (* FIXME: generate a new struct definition on the fly... *)
    ((name, ast) :: rest_ast, marks)

  let object_ ((fields_cases, marks) : fields) =
    (Struct { fields = fields_cases }, marks)

  (* XXX: handling case/union with this walker is weird *)

  let string_const s =
    (* failwith ("unimplemented: string_const " ^ s) *)
    (Constant s, Marks.empty)

  let case key name t =
    Fmt.epr "case key: %a\n" Fmt.(option string) key;
    Fmt.epr "case name: %s\n" name;
    Fmt.epr "case t: %a\n@." Fmt.(pair pp nop) t;
    t

  let union id cases =
    let marks = cases |> List.map ~f:snd |> Marks.merge in
    Fmt.epr "cases: %a@." Fmt.(list (pair pp nop)) cases;

    let constructors =
      cases |> List.map ~f:(function Constant s, _ -> (s, Constant s))
    in
    (* let t, marks = List.unzip cases in (Union t, Marks.merge marks) *)
    (Variant (Option.value_exn id, constructors), marks)

  let any () = failwith "unimpl: nay"

  (* #include <stdbool.h>*)
  (* "bool" *)
  let bool () = make "bool"

  (* XXX *)
  let float () = make "double"
  let date _ = failwith "unimplemented: date "
  let mapping _ = failwith "unimplemented: mapping "

  (* header? *)
  let int64 () = make "int16_t"
  let list _ = failwith "unimplemented: list "
  let null () = failwith "unimplemented: null "
  let nullable _ = failwith "unimplemented: nullable "
  let time _ = failwith "unimplemented: time "
  let time_local _ = failwith "unimplemented: time_local "
  let tuple _ = failwith "unimplemented: tuple "
end)

(* XXX: copied from TypeScript, might be generalizable? *)
let mark_of_schema Schema.{ id; descriptor; parametric } =
  match parametric with
  | None ->
    (Option.value_exn ~here:[%here] id, lazy (Of.descriptor id descriptor))
  | Some parametric -> failwith "no parametrics yet"

let of_schema marks s = of_mark marks @@ mark_of_schema s
let of_schema schema = of_schema Marks.empty schema |> fst
(* End of shameless copy *)

let force fmt _ = Caml.Format.pp_force_newline fmt ()
let non_ptr = [ "int"; "char"; "bool" ]

(* XXX: Butchering the boxing logic while I learn it. *)

let pp ?(indent = 2) fmt ts =
  let rec pp fmt ts =
    let printer =
      let open Fmt in
      let struct_or_union_field f (name, v) =
        let open Fmt in
        let pp = box (Fmt.pair ~sep:sp (hvbox pp) Fmt.string) in
        pp f (v, name ^ ";")
      in

      let variant ?(indent = 2) fmt (name, constructors) =
        let is_basic =
          List.for_all
            ~f:(function _, Constant _ -> true | _ -> false)
            constructors
        in
        let variant_names = List.map ~f:fst constructors in

        (* An enum declaring each constructor of the variant. This is totally
           independent of whether there is a payload associated with those
           constructors. *)
        let enum =
          const string "enum {" ++ force ++ sps indent
          ++ (vbox
             @@ const
                  (list
                     ~sep:(const char ',' ++ force)
                     (fun fmt -> Fmt.fmt "%s_%s" fmt name))
                  variant_names)
          ++ sp ++ const char '}'
        in

        let printer =
          if is_basic then enum else failwith "no support for payloads yet"
        in
        printer fmt ()
      in

      let object_ =
        const string "struct {" ++ force ++ sps indent
        ++ (vbox @@ list ~sep:force struct_or_union_field)
        ++ sp ++ const char '}'
      in
      match ts with
      | Atom s -> const string s
      | Pointer t -> const pp t ++ const char '*'
      | Struct { fields; _ } -> const object_ fields
      | Variant v -> const variant v
    in
    printer fmt ()
  in
  pp fmt ts

let to_c_type_expression = Fmt.to_to_string pp

include C_codegen

let to_of_caml_src name c = Fmt.to_to_string pp_of_caml_converter (name, c)
let to_to_caml_src name c = Fmt.to_to_string pp_to_caml_converter (name, c)

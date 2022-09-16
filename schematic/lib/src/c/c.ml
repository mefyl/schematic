open Base
include C_intf

type c = t

let normalize_id s = s

include Marks.Make (struct
  type t = c

  (* potential FIXME (check valid identifiers in C) *)
  let normalize_id = normalize_id
end)

module Of = Schema.Walk (struct
  type res = t * Marks.t
  type fields = (string * t) list * Marks.t
  type t = res

  let make name = (Atom name, Marks.empty)

  (* Shallow primitive types *)
  let ignore () = make "void"

  (* XXX: bit width *)
  let int () = make "int"
  let string () = failwith "string"

  let schema (id : string option) parametric s =
    Fmt.epr "id: %a\n@." Fmt.(option string) id;
    match parametric with
    | Some parametric -> parametric ()
    | None -> (
      match id with
      | None -> s ()
      | Some id -> (Atom (normalize_id id), Marks.make id s))

  let parametric template_name _args _effective _t =
    Fmt.pr "tn: %s@." template_name;
    failwith "parametric: unimpl"

  let fields_empty () =
    Fmt.pr "fields_empty\n";
    ([], Marks.empty)

  let field_inline _required fields _rest =
    Fmt.pr "fields_inline\n";
    List.hd_exn fields

  let field_outline (name : string) (_required : bool) ((ast, marks) : t)
      ((rest_ast, rest_marks) : fields) =
    Fmt.pr "field_outline (name: %s)\n" name;
    Fmt.pr "field_outline 'ast': %s\n" (show ast);
    Fmt.pr "field_outline 'rest_ast': '%a'\n"
      Fmt.(list @@ pair string pp)
      rest_ast;
    Fmt.pr "\n";

    let marks = Marks.merge [ marks; rest_marks ] in

    let ast =
      match ast with
      | (Atom "int" | Atom "char") as prim -> prim
      | Atom _ as nonprim -> Pointer nonprim
      | _ -> failwith "nah"
    in

    (* FIXME: generate a new struct definition on the fly... *)
    ((name, ast) :: rest_ast, marks)

  let object_ ((fields_cases, marks) : fields) =
    (Struct { fields = fields_cases }, marks)

  let case key name t =
    Fmt.pr "case key: %a" Fmt.(option string) key;
    Fmt.pr "case name: %s" name;
    t

  let union _ = failwith "unimplemented: union "
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
  let string_const _ = failwith "unimplemented: string_const "
  let time _ = failwith "unimplemented: time "
  let time_local _ = failwith "unimplemented: time_local "
  let tuple _ = failwith "unimplemented: tuple "
end)

(* XXX: copied from TypeScript, might be generalizable? *)
let mark_of_schema Schema.{ id; descriptor; parametric } =
  match parametric with
  | None -> (Option.value_exn ~here:[%here] id, lazy (Of.descriptor descriptor))
  | Some parametric ->
    (Option.value_exn ~here:[%here] id, lazy (Of.parametric parametric))

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
      let object_ =
        let field f (name, v) =
          let pp = box (Fmt.pair ~sep:sp (hvbox pp) Fmt.string) in
          pp f (v, name ^ ";")
        in
        const string "struct {" ++ force ++ sps indent
        ++ (vbox @@ list ~sep:force field)
        ++ sp ++ const char '}'
      in
      match ts with
      | Atom s -> const string s
      | Pointer t -> const pp t ++ const char '*'
      | Struct { fields; _ } -> const object_ fields
    in
    printer fmt ()
  in
  pp fmt ts

let to_c_type_expression = Fmt.to_to_string pp

include C_codegen

let to_of_caml_src name c = Fmt.to_to_string pp_of_caml_converter (name, c)
let to_to_caml_src name c = Fmt.to_to_string pp_to_caml_converter (name, c)

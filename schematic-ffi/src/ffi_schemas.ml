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

(* FIXME: factor*)
module State_monad = struct
  module Make (State : sig
    type t
  end) =
  struct
    type 'a t = State.t -> 'a * State.t

    include Base.Monad.Make (struct
      type nonrec 'a t = 'a t

      let return v marks = (v, marks)

      let bind v ~f marks =
        let v, marks = v marks in
        f v marks

      let map =
        let map v ~f marks =
          let v, marks = v marks in
          (f v, marks)
        in
        `Custom map
    end)
  end
end

module Of = Schematic.Schema.Walk (struct
  type t = c
  type case = string * string * t * bool

  module M = State_monad.Make (struct
    type t = Marks.t
  end)

  open M.Let_syntax

  let ( let* ) = ( >>= )
  let ( let+ ) = ( >>| )

  type field = (string * c) option

  (* Built-ins *)

  let ignore = M.return `Unit
  let null = ignore
  let int = M.return `Int
  let int64 = M.return `Int64
  let json = M.return `Json
  let bool = M.return `Bool

  let float =
    (* "OCaml's floating-point numbers follow the IEEE 754 standard, using
       double precision (64 bits) numbers." *)
    M.return `Double

  let string = M.return `String

  let string_const _s =
    (* Mainly used for individual union constructors *)
    M.return `Void

  let any = M.return `Json

  let time marks =
    Lazy.force Ffi_sanity_checks.ptime;
    (`Time, marks)

  (* The rest *)

  (* This will recurse forever if given a cyclic schema. *)
  let schema (id : string option) _ s =
    Logs.debug (fun m -> m "id: %a\n@." Fmt.(option string) id);
    match id with
    | None -> s ()
    | Some id ->
      fun marks ->
        let ((referenced_type, _) as res) = s () marks in
        ( `Atom (Identifier.make id, Some referenced_type),
          Marks.add marks @@ (id, lazy res) )

  let parametric _ _ _ _ = failwith "parametric never used"

  let nullable s =
    let* ast = s in
    let name = option_name ast in
    schema
      (Some (Identifier.to_string name))
      None
      (fun () -> return (`Option ast))

  let field ~required name schema =
    let+ ast = if required then schema else nullable schema in
    Some (name, ast)

  let inline _ schema { Schematic.Schema.name; requirement; _ } =
    Some
      (field ~required:(Schematic.Schema.required requirement) name (schema ()))

  let field ~computed ~required name schema =
    if computed then return None else field ~required name schema

  let object_ fields =
    fields >>| List.map ~f:List.filter_opt >>| function
    | [ [] ] -> `Void
    | [ fields ] ->
      let fields =
        fields |> List.map ~f:(fun (s, c) -> (Identifier.make s, c))
      in
      `Struct { fields }
    | _ -> failwith "unreachable"

  let case ~key:_ ~inline ~name ~raw_name case =
    let+ ast = case in
    (name, raw_name, ast, inline)

  let union ~id ~polymorphic cases =
    let name =
      match id with
      | Some id -> id
      | None -> failwith "union schema must have an id"
    in
    let* cases = cases in
    let constructors =
      cases
      |> List.map ~f:(fun (s, raw_s, c, inline) ->
             (Identifier.make s, raw_s, c, inline))
    in
    let basic, rich =
      constructors
      |> List.partition_map ~f:(function
           | s, raw_s, `Void, _ -> First (s, raw_s)
           | x -> Second x)
    in
    (* Give names to inline payload types that would otherwise be impossible to
       refer to from Swift. We do this by creating extra marks.

       XXX: would be nice to reference those marks in the converter, but as it
       stands, this might break the distinction between inline and regular
       payloads *)
    let+ () =
     fun marks ->
      let inline_payload_marks =
        rich
        |> List.map ~f:(fun (id, _, constr_c, _) ->
               match constr_c with
               | `Tuple _ | `Struct _ ->
                 let id =
                   inline_record_c_name ~variant_name:name ~constructor_name:id
                 in
                 let s () = (constr_c, marks) in
                 Marks.make id s
               | _ -> marks)
        |> Marks.merge
      in
      ((), inline_payload_marks)
    in
    let name = Identifier.make name in
    if polymorphic then
      let basic =
        List.map ~f:(fun (id, raw_name) -> (id, hash_variant raw_name)) basic
      in
      let rich =
        List.map
          ~f:(fun (id, raw_name, c, inline) ->
            assert (not inline);
            { name = id; type_ = c; hash = hash_variant raw_name })
          rich
      in
      `Poly_variant { name; basic; rich }
    else
      let basic = List.map ~f:(fun (id, _) -> id) basic in
      let rich =
        List.map
          ~f:(fun (id, _, c, inline) -> { name = id; type_ = c; inline })
          rich
      in
      `Variant ({ name; basic; rich } : regular_variant)

  let tuple items =
    let* items = items in
    let tuple_name = tuple_name items in
    schema
      (Some (Identifier.to_string tuple_name))
      None
      (fun () -> return (`Tuple items))

  let list s =
    let* ast = s in
    let name = array_name ast in
    schema
      (Some (Identifier.to_string name))
      None
      (fun () -> return (`Array ast))

  (* Simplest possible encoding *)
  (* We could use a struct for "key" / "value" fields instead of a tuple. *)
  let mapping t = tuple (M.all [ string; t ]) |> list

  let map s =
    let+ front_facing =
      let* ast = s in
      match c_canonical_typename ast with
      | Some _ -> M.return ast
      | None ->
        let id = Identifier.to_string (c_typename ast) in
        schema (Some id) None (fun () -> return ast)
    in
    `Mapped front_facing

  (* XXX: make those into built-ins? *)

  (* TODO: for clarity, use a struct *)
  let time_local marks =
    Lazy.force Ffi_sanity_checks.ptime_local;
    tuple (M.all [ time; int ]) marks

  let date marks =
    Lazy.force Ffi_sanity_checks.date;
    let s =
      (* This will work because a Ptime.date is a (int * int * int), which has
         the same memory representation as a struct with three int fields. *)
      let fields =
        [
          (Identifier.make "year", `Int);
          (Identifier.make "month", `Int);
          (Identifier.make "day", `Int);
        ]
      in
      `Struct { fields }
    in
    schema (Some "sch_date") None (fun () -> return s) marks
end)

include Make_mark_converter (struct
  (* FIXME: this is an ugly workaround. *)
  let identifier { Schematic.Schema.id; _ } c =
    match id with
    | Some id -> id
    | None ->
      let id = c_typename c |> Identifier.to_string in
      id ^ "_2"

  let convert id descriptor _ =
    try Of.descriptor ~id descriptor Marks.empty
    with e ->
      Logs.err (fun m ->
          m "Exception thrown for descriptor %a:@ @[%a@]"
            Fmt.(option string)
            id Schematic.Schema.pp_descriptor descriptor);
      raise e
end)

let ctypes_of_schemas_state state schemas = of_schemas_state state schemas

let ctypes_of_schemas schemas =
  ctypes_of_schemas_state state_empty schemas |> fst

let ctypes_of_schema s = ctypes_of_schemas [ Schematic.Schema.pack s ]

open Base
module Identifier = C_identifier

type c_leaf =
  [ `Void
  | `Unit
  | `String
  | `Bool
  | `Int
  | `Int64
  | `Double
  | `Time
  | `Json
  ]
[@@deriving show, hash, eq, ord]

[@@@warning "-30"]

type regular_variant = {
  name : Identifier.t;
  basic : Identifier.t list;
  rich : (Identifier.t * c) list;
}

and poly_variant = {
  name : Identifier.t;
  (* int: hash *)
  basic : (Identifier.t * int) list;
  rich : (Identifier.t * int * c) list;
}

and c_struct = { fields : (Identifier.t * c) list }

and c =
  [ c_leaf
  | `Array of c
  | `Atom of Identifier.t * c option
    (** The RHS is the referenced type, if known. *)
  | `Struct of c_struct
  | `Variant of regular_variant
  | `Poly_variant of poly_variant
  | `Tuple of c list
  | `Mapped of c
  | `Option of c
  ]
[@@deriving show, hash, eq, ord]

[@@@warning "+30"]

type c_function = {
  signature : string;
  impl : unit Fmt.t;
}

type c_gen = {
  name : Identifier.t;
  type_expr : unit Fmt.t;
  to_caml : c_function;
  of_caml : c_function;
}

open Base

let c_builtin_name = function
  | `Void -> "void"
  | `String -> "const char*"
  | `Int -> "int"
  | `Int64 -> "int64_t"
  | `Double -> "double"
  | `Bool -> "bool"
  (* XXX dedup *)
  | `Json -> "sch_json"
  | `Time -> "sch_time"
  | `Unit -> "unit"

let c_builtin_id c = c |> c_builtin_name |> Identifier.make

let rec c_canonical_typename : c -> Identifier.t option = function
  | `String -> Some (Identifier.make "string")
  | `Atom (s, _) -> Some s
  | `Tuple members -> Some (tuple_name members)
  | `Array c -> Some (array_name c)
  | `Variant { name; _ } | `Poly_variant { name; _ } -> Some name
  | `Option c -> Some (option_name c)
  | `Mapped front_facing -> c_canonical_typename front_facing
  | #c_leaf as c -> Some (c_builtin_id c)
  | `Struct _ -> None

and c_typename c =
  match c_canonical_typename c with
  | Some id -> id
  (* Fallback: hash the physical structure of the type. *)
  | None ->
    let s = Fmt.str "_sch_%d" (hash_c c) in
    Logs.debug (fun m -> m "Using hash-based name '%s' for %a" s pp_c c);
    Identifier.make s

and tuple_name items =
  Identifier.make @@ "tuple_"
  ^ (items
    |> List.map ~f:(Fn.compose Identifier.to_string c_typename)
    |> String.concat ~sep:"_")

and array_name t = c_typename t |> Identifier.map ~f:(fun s -> s ^ "_array")
and option_name c = c_typename c |> Identifier.map ~f:(fun s -> s ^ "_option")

let tuple_fields =
  List.mapi ~f:(fun i c -> (Identifier.make @@ Fmt.str "t%d" i, c))

let of_caml_name_for = Identifier.map ~f:(Fmt.str "%s_of_caml")
let to_caml_name_for = Identifier.map ~f:(Fmt.str "%s_to_caml")

let checking_checked_for c =
  ( Fmt.str "sch_%a_checking" Identifier.pp c,
    Fmt.str "sch_%a_checked" Identifier.pp c )

let roundtrip_for = Fmt.str "sch_%a_rt_id" Identifier.pp

(* FIXME: this approach does not protect against collisions *)
let of_caml_name_for_c c = c |> c_typename |> of_caml_name_for
let to_caml_name_for_c c = c |> c_typename |> to_caml_name_for
let value_name = Fmt.str "schematic_%s"

let encode_name_for id serialized =
  let serialized = serialized |> c_typename |> Identifier.to_string in
  Fmt.str "%s_encode_to_%s" id serialized |> value_name

let decode_name_for id serialized =
  let serialized = serialized |> c_typename |> Identifier.to_string in
  Fmt.str "%s_decode_of_%s" id serialized |> value_name

let case_name ~rich variant_name constr_name =
  let suffix = if rich then "_tag" else "" in
  Fmt.str "%a_%a%s" Identifier.pp variant_name Identifier.pp constr_name suffix
  |> Identifier.make

let rec concrete_type = function
  | `Atom (_, Some t) -> concrete_type t
  | `Atom (_, None) -> None
  | t -> Some t

let normalize_poly_variant_members basic rich =
  let basic = List.map ~f:fst basic in
  let rich = List.map ~f:(fun (id, _, c) -> (id, c)) rich in
  (basic, rich)

let inline_record_c_name ~variant_name ~constructor_name =
  Fmt.str "%s__%a" variant_name Identifier.pp constructor_name

let reified_option_c (c : c) =
  `Variant
    ({
       name = option_name c;
       basic = [ Identifier.make "none" ];
       rich = [ (Identifier.make "some", c) ];
     }
      : regular_variant)

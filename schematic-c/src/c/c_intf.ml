open Base
module Identifier = C_identifier

type t =
  | Void
  | String  (** Native string *)
  | Array of t
  | Atom of Identifier.t * t option
      (** The RHS is the referenced type, if known. *)
  | Struct of { fields : (Identifier.t * t) list }
  | Variant of {
      name : Identifier.t;
      basic : Identifier.t list;
      rich : (Identifier.t * t) list;
    }
  | Poly_variant of {
      name : Identifier.t;
      (* int: hash *)
      basic : (Identifier.t * int) list;
      rich : (Identifier.t * int * t) list;
    }
  | Tuple of t list
  | Mapped of { (* "serialized" type *)
                front_facing : t }
[@@deriving show, hash]

type c = t

type c_function = {
  signature : string;
  impl : unit Fmt.t;
}

type c_gen = {
  name : Identifier.t;
  type_expr : unit Fmt.t;
  to_caml : c_function;
  of_caml : c_function; (* TODO free *)
}

open Base

let rec c_typename : c -> Identifier.t = function
  | String -> Identifier.make "string"
  | Atom (s, _) -> s
  | Tuple members -> tuple_name members
  | Array t -> array_name t
  | Variant { name; _ } -> name
  (* Fallback: hash the physical structure of the type. *)
  | c -> Identifier.make (Fmt.str "_sch_%d" (hash c))

and tuple_name items =
  Identifier.make @@ "tuple_"
  ^ (items
    |> List.map ~f:(Fn.compose Identifier.to_string c_typename)
    |> String.concat ~sep:"_")

and array_name t = c_typename t |> Identifier.map ~f:(fun s -> s ^ "_array")

let tuple_fields =
  List.mapi ~f:(fun i c -> (Identifier.make @@ Fmt.str "t%d" i, c))

let of_caml_name_for = Identifier.map ~f:(Fmt.str "%s_of_caml")
let to_caml_name_for = Identifier.map ~f:(Fmt.str "%s_to_caml")

let checking_checked_for c =
  ( Fmt.str "sch_%a_checking" Identifier.pp c,
    Fmt.str "sch_%a_checked" Identifier.pp c )

let roundtrip_for = Fmt.str "sch_%a_rt_id" Identifier.pp

(* FIXME: this approach does not protect against collisions *)
let of_caml_name_for_c = Fn.compose of_caml_name_for c_typename
let to_caml_name_for_c = Fn.compose to_caml_name_for c_typename
let value_name = Fmt.str "schematic_%s"
let encode_name_for typ = Fmt.str "%s_encode" typ |> value_name
let decode_name_for typ = Fmt.str "%s_decode" typ |> value_name

let encode_name_for_c c =
  c_typename c |> Identifier.to_string |> encode_name_for

let decode_name_for_c c =
  c_typename c |> Identifier.to_string |> decode_name_for

let case_name ~rich variant_name constr_name =
  let suffix = if rich then "_tag" else "" in
  Fmt.str "%a_%a%s" Identifier.pp variant_name Identifier.pp constr_name suffix
  |> Identifier.make

let rec concrete_type = function
  | Atom (_, Some t) -> concrete_type t
  | Atom (_, None) -> None
  | t -> Some t

let normalize_poly_variant_members basic rich =
    let basic = List.map ~f:fst basic in
    let rich = List.map ~f:(fun (id, _, c) -> (id, c)) rich in
    (basic, rich)

open Base
open Ppxlib

module Ast_pattern = struct
  include Ast_pattern

  let ( let+ ) v f = map1 ~f v

  let drop () =
    let f _ _ _ cont = cont in
    of_func f
end

module List = struct
  include List

  let fold_righti ~f ~init =
    let rec fold i = function
      | [] -> init
      | hd :: tl ->
        let tl = fold (i + 1) tl in
        f i tl hd
    in
    fold 0
end

let param_prefix = "__schematic_param_"
let expect ~loc pattern ast = Ast_pattern.parse pattern loc ast Fn.id

let rename name =
  let len = String.length name in
  if Char.(name.[len - 1] = '_') then String.sub name ~pos:0 ~len:(len - 1)
  else name

let name suffix s =
  let s = rename s in
  if String.(s = "t") then suffix else s ^ "_" ^ suffix

let suffix_id ~loc ?(prefix = "") suffix = function
  | Lident n -> Lident (prefix ^ name suffix n)
  | Ldot (q, n) -> Ldot (q, prefix ^ name suffix n)
  | Lapply (_, _) ->
    Ppxlib.Location.raise_errorf ~loc "Functor application found as a type"

let versioned versioned unversioned =
  let open Ast_pattern in
  let versioned =
    let+ op, (schema, version) =
      let version v =
        let f context loc pl cont =
          parse (estring __) loc pl (fun s ->
              match Semver.of_string s with
              | None -> Location.raise_errorf ~loc "invalid semantic version"
              | Some version ->
                (to_func v) context loc version (fun v -> cont v))
        in
        of_func f
      and op v =
        let f context loc pl cont =
          let v = to_func v in
          parse
            (pexp_ident @@ lident __)
            loc pl
            (function
              | "=" -> v context loc `Eq (fun v -> cont v)
              | "<" -> v context loc `Lt (fun v -> cont v)
              | "<=" -> v context loc `Lte (fun v -> cont v)
              | ">" -> v context loc `Gt (fun v -> cont v)
              | ">=" -> v context loc `Gte (fun v -> cont v)
              | op ->
                Location.raise_errorf ~loc "invalid version operator: %s" op)
        in
        of_func f
      in
      pexp_apply (op __)
        (pair nolabel versioned ^:: pair nolabel (version __) ^:: nil |> pack2)
      |> pack2
    in
    (schema, Some (version, op))
  and unversioned =
    let+ expr = unversioned in
    (expr, None)
  in
  single_expr_payload (versioned ||| unversioned)

let version_match ~f init options =
  let f next = function
    | { txt = schema, None; _ } -> f schema
    | {
        txt =
          schema, Some (Semver.{ major; minor; patch; prerelease; build }, op);
        loc;
      } ->
      let module Ast_builder = Ast_builder.Make (struct
        let loc = loc
      end) in
      let version =
        [%expr
          Schematic.Version.make
            ~prerelease:
              [%e
                Ast_builder.elist @@ List.map ~f:Ast_builder.estring prerelease]
            ~build:
              [%e Ast_builder.elist @@ List.map ~f:Ast_builder.estring build]
            [%e Ast_builder.eint major] [%e Ast_builder.eint minor]
            [%e Ast_builder.eint patch]]
      and predicate =
        match op with
        | `Eq -> [%expr Schematic.Version.equal]
        | `Gt -> [%expr Schematic.Version.greater_than]
        | `Gte ->
          [%expr
            fun l r ->
              Schematic.Version.equal l r || Schematic.Version.greater_than l r]
        | `Lt -> [%expr Schematic.Version.less_than]
        | `Lte ->
          [%expr
            fun l r ->
              Schematic.Version.equal l r || Schematic.Version.less_than l r]
      in
      [%expr
        match __schematic_schema_version with
        | Some version when [%e predicate] version [%e version] -> [%e f schema]
        | _ -> [%e next]]
  in
  List.fold ~init ~f options

let expr_option ~loc expr = function
  | None -> [%expr None]
  | Some e -> [%expr Some [%e expr ~loc e]]

(* Caml variant constructor of the form [Foo of int * int] surprisingly do not
   have one tuple argument, but two individual int arguments, while [Foo of (int
   * int)] has a single tuple argument. Map the first case to the second and
   back.

   While we're at it, map constructor record arguments to tuple too, to
   streamline constructors arguments handling. *)

type normalized_ctor = {
  ctor_pattern : (int -> pattern) -> pattern;
  ctor_value : (int -> core_type -> expression) -> expression;
  inline : bool;
  pattern : (int -> pattern) -> pattern;
  type_ : core_type;
  value : (int -> core_type -> expression) -> expression;
}

let constructor_arguments_normalize ~loc =
  let module Ast = (val Ast_builder.make loc) in
  function
  | Pcstr_tuple [] | Pcstr_record [] -> None
  | Pcstr_tuple [ type_ ] ->
    let pattern f = f 0
    and value f = f 0 type_ in
    Some
      {
        ctor_pattern = pattern;
        ctor_value = value;
        inline = false;
        pattern;
        type_;
        value;
      }
  | Pcstr_tuple types ->
    let type_ = Ast.ptyp_tuple types
    and pattern f =
      let f i _ = f i in
      Ast.ppat_tuple (List.mapi ~f types)
    and value f = Ast.pexp_tuple (List.mapi ~f types) in
    Some
      {
        ctor_pattern = pattern;
        ctor_value = value;
        inline = true;
        pattern;
        type_;
        value;
      }
  | Pcstr_record fields ->
    let types =
      let f { pld_type; _ } = pld_type in
      List.map ~f fields
    and ctor_pattern f =
      Ast.ppat_record
        (List.mapi
           ~f:(fun i v -> ({ txt = Lident v.pld_name.txt; loc }, f i))
           fields)
        Closed
    and ctor_value f =
      Ast.pexp_record
        (List.mapi
           ~f:(fun i { pld_name; pld_type; pld_loc = loc; _ } ->
             ({ txt = Lident pld_name.txt; loc }, f i pld_type))
           fields)
        None
    in
    let type_, pattern, value =
      match types with
      | [ type_ ] ->
        let pattern f = f 0
        and value f = f 0 type_ in
        (type_, pattern, value)
      | types ->
        let pattern f =
          let f i _ = f i in
          Ast.ppat_tuple (List.mapi ~f types)
        and value f = Ast.pexp_tuple (List.mapi ~f types) in
        (Ast.ptyp_tuple types, pattern, value)
    in
    Some { ctor_pattern; ctor_value; inline = true; pattern; type_; value }

module Warning = struct
  let make ~loc n =
    let module Ast = (val Ast_builder.make loc) in
    Ast.attribute ~name:{ txt = "warning"; loc }
      ~payload:
        (PStr [ { pstr_desc = Pstr_eval (Ast.estring n, []); pstr_loc = loc } ])

  let unused_var_strict ~loc = make ~loc "-unused-var-strict"
  let unused_rec ~loc = make ~loc "-unused-rec-flag"
end

let unused ~loc t =
  let open (val Ast_builder.make loc) in
  {
    t with
    ptype_attributes =
      {
        attr_name = Located.mk "warning";
        attr_payload =
          PStr [ pstr_eval (estring "-unused-type-declaration") [] ];
        attr_loc = loc;
      }
      :: t.ptype_attributes;
  }

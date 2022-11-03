open C_intf
open Base

let pp ?(indent = 2) fmt (ts : c) =
  let rec pp fmt (ts : c) =
    let printer =
      let open Fmt in
      let open C_pp in
      let struct_or_union_field f (name, v) =
        let open Fmt in
        let pp = box (Fmt.pair ~sep:sp (hvbox pp) Fmt.string) in
        pp f (v, Identifier.to_string name ^ ";")
      in
      let struct_ : (Identifier.t * _) list Fmt.t =
        block ~indent "struct" (list ~sep:cut struct_or_union_field)
      in
      let union_ : (Identifier.t * _) list Fmt.t =
        block ~indent "union" (list ~sep:cut struct_or_union_field)
      in
      let variant fmt (name, basic, rich) =
        let is_basic = List.is_empty rich in
        (* An enum declaring each constructor of the variant. *)
        let enum =
          let variant_names = basic @ List.map ~f:fst rich in
          block ~indent "enum"
            (const
               (list
                  ~sep:(const char ',' ++ cut)
                  (fun fmt s ->
                    Fmt.fmt "%a" fmt Identifier.pp
                      (case_name ~rich:(not is_basic) name s)))
               variant_names)
        in
        let printer =
          if is_basic then enum
          else
            let tag_field = enum ++ const string " tag;" in
            let payload_field =
              const union_ rich ++ const string "* payload;"
            in
            let inner = tag_field ++ cut ++ payload_field in
            block ~indent "struct" inner
        in
        printer fmt ()
      in
      let array =
        (* We're not using the `struct_` formatter, on purpose. *)
        block ~indent "struct"
          (const string "size_t len" ++ semi ++ pp ++ const string "* "
         ++ const string "data;")
      in
      match ts with
      | `Option c -> const pp (reified_option_c c)
      | `Atom (s, _) -> const string (Identifier.to_string s)
      | `Array t -> const array t
      | `Struct { fields; _ } -> const struct_ fields
      (* `Tuples are structs with types named according to their indices *)
      | `Tuple types ->
        let fields = tuple_fields types in
        const struct_ fields
      | `Variant { name; basic; rich; _ } -> const variant (name, basic, rich)
      | `Poly_variant { name; basic; rich; _ } ->
        let basic, rich = normalize_poly_variant_members basic rich in
        const variant (name, basic, rich)
      | `Mapped front_facing -> const pp front_facing
      | #c_leaf as c -> const string (c_builtin_name c)
    in
    printer fmt ()
  in
  pp fmt ts

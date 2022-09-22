open C_intf
open Base

let pp ?(indent = 2) fmt ts =
  let rec pp fmt ts =
    let printer =
      let open Fmt in
      (* TODO: get rid of this "braced" (redundant with C_pp) *)
      let braced lhs inner : _ Fmt.t =
        (vbox ~indent @@ (lhs ++ const string " {" ++ cut ++ inner))
        ++ cut ++ const char '}'
      in

      let struct_or_union_field f (name, v) =
        let open Fmt in
        let pp = box (Fmt.pair ~sep:sp (hvbox pp) Fmt.string) in
        pp f (v, Identifier.to_string name ^ ";")
      in

      let struct_ : (Identifier.t * _) list Fmt.t =
        braced (const string "struct") (list ~sep:cut struct_or_union_field)
      in

      let union_ : (Identifier.t * _) list Fmt.t =
        braced (const string "union") (list ~sep:cut struct_or_union_field)
      in

      let variant fmt (name, basic, rich) =
        let is_basic = List.is_empty rich in
        (* An enum declaring each constructor of the variant. *)
        let enum =
          let variant_names =
            (basic |> List.map ~f:Identifier.to_string)
            @ List.map ~f:(Fn.compose Identifier.to_string fst) rich
          in
          let tag_suffix = if is_basic then "" else "_tag" in
          braced (const string "enum")
            (const
               (list
                  ~sep:(const char ',' ++ cut)
                  (fun fmt s ->
                    Fmt.fmt "%a_%s%s" fmt Identifier.pp name s tag_suffix))
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

            (* Kind of a special case of struct... *)
            braced (const string "struct") inner
        in
        printer fmt ()
      in

      match ts with
      | Atom (s, Some (Struct _)) ->
        const string (Identifier.to_string s) ++ const char '*'
      | Atom (s, _) -> const string (Identifier.to_string s)
      | Array t ->
        (* We're not using the `struct_` formatter, on purpose. *)
        C_pp.braced ~indent "struct"
          (const string "size_t len" ++ semi ++ const pp t ++ const string "* "
         ++ const string "data;")
      | String -> const string "char*"
      | Struct { fields; _ } -> const struct_ fields
      (* Tuples are structs with types named according to their indices *)
      | Tuple types ->
        let fields = tuple_fields types in
        const struct_ fields
      | Variant { name; basic; rich } -> const variant (name, basic, rich)
      | Void -> nop
    in

    printer fmt ()
  in
  pp fmt ts

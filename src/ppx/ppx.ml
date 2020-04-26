open Batteries
open Ppxlib

let pattern_id ~loc txt =
  { ppat_desc = Ppat_var { txt; loc }
  ; ppat_loc = loc
  ; ppat_loc_stack = []
  ; ppat_attributes = []
  }

let expr_id ~loc txt =
  { pexp_desc = Pexp_ident { txt; loc }
  ; pexp_loc = loc
  ; pexp_loc_stack = []
  ; pexp_attributes = []
  }

let expr_apply ~loc f args =
  { pexp_desc = Pexp_apply (f, args)
  ; pexp_loc = loc
  ; pexp_loc_stack = []
  ; pexp_attributes = []
  }

let expr_list ~loc list =
  List.fold_right (fun m l -> [%expr [%e m] :: [%e l]]) list [%expr []]

let schema_name s = s ^ "_schema"

let schema_id = function
  | Lident name -> Lident (schema_name name)
  | Ldot (q, name) -> Ldot (q, schema_name name)
  | Lapply (_, _) -> failwith "Lapply"

let expr_string ~loc txt =
  { pexp_desc = Pexp_constant (Pconst_string (txt, None))
  ; pexp_loc = loc
  ; pexp_loc_stack = []
  ; pexp_attributes = []
  }

let rename name =
  let len = String.length name in
  if name.[len - 1] = '_' then
    String.sub name 0 (len - 1)
  else
    name

let rec encode_type ~loc = function
  (* Mapping *)
  | [%type: (string * [%t? t]) list] ->
    [%expr assoc_schema [%e encode_type ~loc t]]
  (* Plain type *)
  | { ptyp_desc = Ptyp_constr ({ txt; loc }, []); _ } ->
    [%expr [%e expr_id ~loc (schema_id txt)]]
  (* Parametric type *)
  | { ptyp_desc = Ptyp_constr ({ txt; loc }, args); _ } ->
    let schema = [%expr [%e expr_id ~loc (schema_id txt)]]
    and f t = (Nolabel, encode_type ~loc t) in
    { pexp_desc = Pexp_apply (schema, List.map f args)
    ; pexp_loc = loc
    ; pexp_loc_stack = []
    ; pexp_attributes = []
    }
  (* Tuple *)
  | { ptyp_desc = Ptyp_tuple types; _ } ->
    let field_name i { ptyp_loc = loc; _ } = (loc, "e" ^ Int.to_string i)
    and constructed =
      { pexp_desc =
          Pexp_tuple
            (List.mapi
               (fun i _ -> expr_id ~loc (Lident ("e" ^ Int.to_string i)))
               types)
      ; pexp_loc = loc
      ; pexp_loc_stack = []
      ; pexp_attributes = []
      }
    and deconstructed =
      { ppat_desc =
          Ppat_tuple
            (List.mapi
               (fun i _ -> pattern_id ~loc ("e" ^ Int.to_string i))
               types)
      ; ppat_loc = loc
      ; ppat_loc_stack = []
      ; ppat_attributes = []
      }
    in
    combinator ~loc types field_name constructed deconstructed
      (tuple_schema ~loc types)
  (* Type variable *)
  | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ } ->
    [%expr [%e expr_id ~loc (schema_id (Lident name))]]
  (* Unsupported *)
  | core_type ->
    Ppxlib.Location.raise_errorf ~loc "schematic-ppx: non supported type %a"
      Pprintast.core_type core_type

and tuple_schema ~loc types =
  let args = List.map (encode_type ~loc) types in
  let init = [%expr Null] in
  let f l r = [%expr Tuple { schema = [%e r]; rest = [%e l] }] in
  List.fold_left f init args

and combinator :
    type f.
       loc:location
    -> f list
    -> (int -> f -> location * label)
    -> expression
    -> pattern
    -> expression
    -> expression =
 fun ~loc fields field_name constructed deconstructed obj ->
  let construct =
    let pattern =
      let pattern i f =
        let loc, name = field_name i f in
        pattern_id ~loc name
      in
      let f i field tl =
        let pattern = pattern i field in
        [%pat? [%p pattern], [%p tl]]
      in
      List.fold_righti f fields [%pat? ()]
    in
    { pexp_desc = Pexp_fun (Nolabel, None, pattern, constructed)
    ; pexp_loc = loc
    ; pexp_loc_stack = []
    ; pexp_attributes = []
    }
  and deconstruct =
    let value =
      let expr i f =
        let loc, name = field_name i f in
        expr_id ~loc (Lident name)
      in
      let f i field tl =
        let expr = expr i field in
        [%expr [%e expr], [%e tl]]
      in
      List.fold_righti f fields [%expr ()]
    in
    { pexp_desc = Pexp_fun (Nolabel, None, deconstructed, value)
    ; pexp_loc = loc
    ; pexp_loc_stack = []
    ; pexp_attributes = []
    }
  in
  [%expr Map ([%e construct], [%e deconstruct], [%e obj])]

let expand ~loc ~path:_ (_, types) =
  let f t =
    let name = schema_name t.ptype_name.txt in
    let encoder =
      let body =
        match t.ptype_kind with
        | Ptype_record fields ->
          let schema =
            let args =
              let f = function
                | { pld_attributes
                  ; pld_loc = loc
                  ; pld_name = { txt; _ }
                  ; pld_type
                  ; _
                  } -> (
                  let f txt { attr_name; attr_loc = loc; attr_payload } =
                    match attr_name.txt with
                    | "schematic.as" -> (
                      match attr_payload with
                      | PStr
                          [%str
                            [%e?
                              { pexp_desc =
                                  Pexp_constant (Pconst_string (txt, None))
                              ; _
                              }]] ->
                        txt
                      | _ ->
                        Ppxlib.Location.raise_errorf ~loc
                          "schematic-ppx: string payload expected" )
                    | name when String.starts_with name "schematic." ->
                      Ppxlib.Location.raise_errorf ~loc
                        "schematic-ppx: unknown schematic annotation %s" name
                    | _ -> txt
                  in
                  let txt = List.fold_left f txt pld_attributes in
                  match pld_type with
                  | { ptyp_desc =
                        Ptyp_constr ({ txt = Lident "option"; _ }, [ t ])
                    ; _
                    } ->
                    ( [%expr Optional]
                    , expr_string ~loc (rename txt)
                    , encode_type ~loc t )
                  | t ->
                    ( [%expr Required]
                    , expr_string ~loc (rename txt)
                    , encode_type ~loc t ) )
              in
              List.map f fields
            in
            let init = [%expr Null] in
            let f (requirement, field, schema) rest =
              [%expr
                Object
                  { field = [%e field]
                  ; title = None
                  ; description = None
                  ; schema = [%e schema]
                  ; requirement = [%e requirement]
                  ; rest = [%e rest]
                  }]
            in
            List.fold_right f args init
          and field_name _ { pld_name = { txt; loc }; _ } = (loc, txt)
          and constructed =
            let f { pld_name = { txt; loc }; _ } =
              ( { txt = Lident txt; loc }
              , { pexp_desc = Pexp_ident { txt = Lident txt; loc }
                ; pexp_loc = loc
                ; pexp_loc_stack = []
                ; pexp_attributes = []
                } )
            in
            { pexp_desc = Pexp_record (List.map f fields, None)
            ; pexp_loc = loc
            ; pexp_loc_stack = []
            ; pexp_attributes = []
            }
          and deconstructed =
            let f { pld_name = { txt; loc }; _ } =
              ( { txt = Lident txt; loc }
              , { ppat_desc = Ppat_var { txt; loc }
                ; ppat_loc = loc
                ; ppat_loc_stack = []
                ; ppat_attributes = []
                } )
            in
            { ppat_desc = Ppat_record (List.map f fields, Closed)
            ; ppat_loc = loc
            ; ppat_loc_stack = []
            ; ppat_attributes = []
            }
          in
          combinator ~loc fields field_name constructed deconstructed schema
        | Ptype_abstract -> (
          match t.ptype_manifest with
          | Some t -> encode_type ~loc t
          | None -> failwith "no manifest on abstract type" )
        | Ptype_open -> [%expr 51]
        | Ptype_variant options ->
          let f { pcd_name = { txt; loc = _ }; pcd_args; pcd_loc = loc; _ } =
            let schema =
              match pcd_args with
              | Pcstr_tuple [ field ] -> encode_type ~loc field
              | Pcstr_tuple fields -> tuple_schema ~loc fields
              | Pcstr_record _ -> failwith "not implemented"
            and construct =
              let pattern =
                { ppat_desc =
                    Ppat_construct
                      ( { txt = Lident txt; loc }
                      , Some
                          { ppat_desc = Ppat_var { txt = "x"; loc }
                          ; ppat_loc = loc
                          ; ppat_loc_stack = []
                          ; ppat_attributes = []
                          } )
                ; ppat_loc = loc
                ; ppat_loc_stack = []
                ; ppat_attributes = []
                }
              in
              [%expr
                function
                | [%p pattern] -> Some x
                | _ -> None]
            and destruct =
              let value =
                { pexp_desc =
                    Pexp_construct
                      ( { txt = Lident txt; loc }
                      , Some
                          { pexp_desc = Pexp_ident { txt = Lident "x"; loc }
                          ; pexp_loc = loc
                          ; pexp_loc_stack = []
                          ; pexp_attributes = []
                          } )
                ; pexp_loc = loc
                ; pexp_loc_stack = []
                ; pexp_attributes = []
                }
              in
              [%expr
                function
                | x -> [%e value]]
            in
            [%expr
              Case
                { schema = [%e schema]
                ; encoder = [%e construct]
                ; decoder = [%e destruct]
                }]
          in
          [%expr Union [%e expr_list ~loc (List.map f options)]]
      and wrap arg exp =
        match arg with
        | { ptyp_desc = Ptyp_var name; ptyp_loc = loc; _ }, Invariant ->
          [%expr fun [%p pattern_id ~loc (schema_name name)] -> [%e exp]]
        | t, _ ->
          Ppxlib.Location.raise_errorf ~loc
            "schematic-ppx: non supported type parameter %a" Pprintast.core_type
            t
      in
      List.fold_right wrap t.ptype_params body
    and pattern = pattern_id ~loc:t.ptype_name.loc name in
    [%str
      let rec [%p pattern] =
        let open Schematic.Schema in
        let _ = int_schema in
        let _ = [%e expr_id ~loc (Lident name)] in
        [%e encoder]]
  in
  List.map f types |> List.concat

let str_type_decl_generator =
  Deriving.Generator.make_noarg ~attributes:[] expand

let my_deriver = Deriving.add ~str_type_decl:str_type_decl_generator "schema"

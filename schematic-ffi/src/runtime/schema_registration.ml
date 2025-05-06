open Base
open C_intf
open Schematic

let register_map id ~serialized ~encode ~decode =
  let encode_name = encode_name_for id serialized in
  let decode_name = decode_name_for id serialized in
  Logs.info (fun m ->
      m
        {|Registering Map encode/decode for '%s':
          encoder: '%s'
          decoder: '%s'|}
        id encode_name decode_name);
  Stdlib.Callback.register encode_name encode;
  Stdlib.Callback.register decode_name (Fn.compose Result.ok_or_failwith decode)

let rec traverse_descriptor : type t.
    string option -> t Schema.descriptor -> unit =
 fun id d ->
  match d with
  (* Shallow types *)
  | Null | String | Time | Bool | Float | Int | Ignore | Int64 | Time_local
  | String_const _ | Date | Any _ | Json ->
    ()
  | Coding { f } -> traverse_descriptor id (f None)
  | Fixpoint d -> traverse_descriptor id (Lazy.force d)
  | List s -> traverse_schema s
  | Sequence { item; _ } -> traverse_schema item
  | Map { descriptor = serialized_d; encode; decode } as map_d ->
    let id : string =
      match id with
      | Some id -> id
      | None ->
        Fmt.failwith
          {|Mapped descriptor '%a' may not be used without a name, to prevent unsoundness.|}
          Schematic.Schema.pp_descriptor map_d
    in
    let serialized, _ = Ffi_schemas.of_descriptor serialized_d in
    let id = id |> Identifier.normalize_s in
    register_map id ~serialized ~encode ~decode;
    traverse_descriptor None serialized_d
  | Mapping { item; _ } -> traverse_schema item
  | Nullable s -> traverse_schema s
  | Object { fields; _ } -> traverse_fields fields
  | Tuple { items; _ } -> traverse_tuple items
  | Union { cases; _ } ->
    cases
    |> List.iter ~f:(fun (Schema.Case { schema = s; _ }) -> traverse_schema s)

and traverse_schema : type t. t Schema.t -> unit =
 fun { id; descriptor = d; parametric = p } ->
  traverse_descriptor id d;
  match p with None -> () | Some p -> traverse_parametric p

and traverse_tuple : type t. t Schema.tuple -> unit = function
  | ItemEnd -> ()
  | Item { schema = s; rest } ->
    traverse_schema s;
    traverse_tuple rest

and traverse_parametric : type t. t Schema.parametric -> unit = function
  | Plain { name; descriptor = d } -> traverse_descriptor (Some name) d
  | Parametric { parameter; next; _ } ->
    traverse_parametric (next { parameter with parametric = None })

and traverse_fields : type t. t Schema.fields -> unit = function
  | FieldEnd -> ()
  | Field
      {
        field =
          {
            schema =
              ( Outline { descriptor = Ignore; _ }
              | Inline { descriptor = Ignore; _ }
              | Inline_try { descriptor = Ignore; _ } );
            _;
          };
        rest;
      } ->
    traverse_fields rest
  | Field { field = { omit = true; _ }; rest } -> traverse_fields rest
  | Field { field = { schema = Outline s | Inline s | Inline_try s; _ }; rest }
    ->
    traverse_fields rest;
    traverse_schema s

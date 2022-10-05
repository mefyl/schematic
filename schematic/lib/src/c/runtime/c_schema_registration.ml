open Base
open C_intf

let register_map type_name ~encode ~decode =
  Stdlib.Callback.register (encode_name_for type_name) encode;
  let decode = Fn.compose Result.ok_or_failwith decode in
  Stdlib.Callback.register (decode_name_for type_name) decode

let rec traverse_descriptor :
    type t. string option -> t Schema_intf.descriptor -> unit =
 fun id d ->
  match d with
  (* Shallow types *)
  | Null | String | Time | Bool | Float | Int | Ignore | Int64 | Time_local
  | String_const _ | Date | Any ->
    ()
  | Fixpoint d -> traverse_descriptor id (Lazy.force d)
  | List s -> traverse_schema s
  | Map { descriptor = d; encode; decode } ->
    let id =
      let c, _ = C_schemas.of_descriptor None d in
      let default = c |> c_typename |> Identifier.to_string in
      (Option.map id ~f:(Fn.compose Identifier.to_string Identifier.make)) |> Option.value ~default
    in
    Logs.info (fun m -> m "Registering Map encode/decode for '%s'" id);
    register_map id ~encode ~decode;
    traverse_descriptor (Some id) d
  | Mapping s -> traverse_schema s
  | Nullable s -> traverse_schema s
  | Object { fields; _ } -> traverse_fields fields
  | Tuple { items; _ } -> traverse_tuple items
  | Union { cases; _ } ->
    cases
    |> List.iter ~f:(fun (Schema.Case { schema = s; _ }) -> traverse_schema s)

and traverse_schema : type t. t Schema_intf.t -> unit =
 fun { id; descriptor = d; parametric = p } ->
  traverse_descriptor id d;
  match p with None -> () | Some p -> traverse_parametric p

and traverse_tuple : type t. t Schema_intf.tuple -> unit = function
  | ItemEnd -> ()
  | Item { schema = s; rest } ->
    traverse_schema s;
    traverse_tuple rest

and traverse_parametric : type t. t Schema_intf.parametric -> unit = function
  | Plain { name; descriptor = d } -> traverse_descriptor (Some name) d
  | Parametric { parameter; next; _ } ->
    traverse_parametric (next { parameter with parametric = None })

and traverse_fields : type t. t Schema_intf.fields -> unit = function
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

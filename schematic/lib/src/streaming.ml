include Streaming_intf
module E = Error
open Base
open Schemas

type date_tuple = int * int * int [@@deriving schema]
type ('a, 'b) pair = 'a * 'b [@@deriving schema]

let mapping_schema element_schema =
  list_schema @@ pair_schema string_schema element_schema

type time_local_pair = Ptime.t * int [@@deriving schema]

module Make (Impl : IMPL) = struct
  let name = Impl.name

  module Input = Impl.Input
  module Output = Impl.Output

  type 'a reader = ('a, Impl.Input.t) Reader.t

  let requirement_return (type a req) (req : (a, req) Schema.requirement)
      (v : a) : req =
    match req with
    | Computed -> Some v
    | Computed_optional -> Some (Some v)
    | Default _ -> v
    | Optional -> Some v
    | Required -> v

  let fail reason = raise (Content { E.Decoding.empty with E.Decoding.reason })
  let failf fmt = Fmt.kstr fail fmt

  let read schema reader =
    try
      let module R = Impl.Input in
      let rec read_descriptor : type t. t Schema.descriptor -> t reader =
       fun descriptor reader ->
        match descriptor with
        | Any (module Any) ->
          failf "any %s descriptor not supported when streaming" Any.name
        | Bool -> R.bool reader
        | Coding { f } -> read_descriptor (f None) reader
        | Date -> (
          match R.date with
          | `Generic -> read_schema date_tuple_schema reader
          | `Custom f -> f reader)
        | Fixpoint s -> read_descriptor (Lazy.force s) reader
        | Float -> R.float reader
        | Ignore -> ()
        | Int -> R.int reader
        | Int64 -> R.int64 reader
        | Json -> R.json reader
        | List items_schema -> (
          match R.list_push reader with
          | Some size ->
            let items =
              let rec read_items i acc reader =
                if i = 0 then List.rev acc
                else
                  match R.list_item_push reader with
                  | false -> fail "missing list item"
                  | true ->
                    let head = read_schema items_schema reader in
                    let () = R.list_item_pop reader in
                    read_items (i - 1) (head :: acc) reader
              in
              read_items size [] reader
            in
            let () = R.list_pop reader in
            items
          | None ->
            let items =
              let rec read_items acc reader =
                match R.list_item_push reader with
                | false -> List.rev acc
                | true ->
                  let head = read_schema items_schema reader in
                  let () = R.list_item_pop reader in
                  read_items (head :: acc) reader
              in
              read_items [] reader
            in
            let () = R.list_pop reader in
            items)
        | Map { decode; descriptor; _ } -> (
          let value = read_descriptor descriptor reader in
          match decode value with
          | Result.Error error -> fail error
          | Result.Ok value -> value)
        | Mapping items_schema ->
          read_schema (mapping_schema items_schema) reader
        | Null -> ()
        | Nullable schema -> (
          match R.option_push reader with
          | false -> None
          | true ->
            let value = read_schema schema reader in
            let () = R.option_pop reader in
            Some value)
        | Object { fields = fields_schema; decode = decode_object; _ } ->
          let () = R.object_push reader in
          let fields =
            let rec read_field : type t. t Hlist.t Schema.fields -> t Hlist.t =
              function
              | FieldEnd -> Hlist.[]
              | Field { field; rest } -> (
                match Schema.field_schema field.schema with
                | { descriptor = Ignore; _ } ->
                  let rest = read_field rest in
                  Hlist.(requirement_return field.requirement () :: rest)
                | schema ->
                  let head =
                    match field.requirement with
                    | Default _ | Optional | Required -> (
                      let some =
                        match field.requirement with
                        | Optional -> R.object_field_opt_push reader
                        | _ ->
                          let () = R.object_field_push reader in
                          true
                      in
                      if some then
                        let head = read_schema schema reader in
                        let () =
                          match field.requirement with
                          | Optional -> R.object_field_opt_pop reader
                          | _ -> R.object_field_pop reader
                        in
                        requirement_return field.requirement head
                      else
                        match field.requirement with
                        | Computed -> (* unreachable *) None
                        | Computed_optional -> (* unreachable *) None
                        | Default value -> value
                        | Optional -> None
                        | Required ->
                          failf "missing required field %S" field.name)
                    | Computed -> None
                    | Computed_optional -> None
                  in
                  let tail = read_field rest in
                  Hlist.(head :: tail))
            in
            read_field fields_schema
          in
          let () = R.object_pop reader in
          decode_object fields
        | String -> R.string reader
        | String_const _ -> ()
        | Time -> (
          match R.time with
          | `Generic -> (
            let timestamp = read_descriptor Float reader in
            match Ptime.of_float_s timestamp with
            | Some time -> time
            | None -> failf "timestamp does not fit: %f" timestamp)
          | `Custom f -> f reader)
        | Time_local -> (
          match R.time_local with
          | `Generic -> read_schema time_local_pair_schema reader
          | `Custom f -> f reader)
        | Tuple { items; decode = decode_tuple; _ } ->
          let () = R.tuple_push reader in
          let items =
            let rec decode_tuple : type t. int -> t Schema.tuple -> t =
             fun i -> function
              | ItemEnd ->
                let () = R.tuple_pop reader in
                Hlist.[]
              | Item { schema; rest } ->
                let () = R.tuple_item_push reader in
                let head = read_schema schema reader in
                let () = R.tuple_item_pop reader in
                let tail = decode_tuple (i + 1) rest in
                Hlist.(head :: tail)
            in
            decode_tuple 0 items
          in
          decode_tuple items
        | Union { cases; _ } ->
          let (Case case) = R.union_push cases reader in
          let value = read_schema case.schema reader in
          let () = R.union_pop reader in
          case.decoder value
      and read_schema : type t. t Schema.t -> t reader =
       fun schema input -> read_descriptor schema.descriptor input
      in
      read_schema schema reader |> Result.return
    with Content error -> Result.Error error

  let of_channel schema channel =
    let input = Impl.Input.of_channel channel in
    read schema input

  let of_string schema string =
    let read reader =
      let ( let* ) = Result.( >>= ) in
      let* result = read schema reader in
      let* () =
        try Impl.Input.eof reader |> Result.return
        with _ ->
          Result.Error { E.schema = schema.Schema.id; path = []; reason = "" }
      in
      Result.Ok result
    in
    let input = Impl.Input.of_string string in
    let result = read input in
    result

  type 'a writer = ('a, Impl.Output.t) Writer.t

  let write schema v writer =
    let module W = Impl.Output in
    let rec write_descriptor :
        type t. E.decoding -> t Schema.descriptor -> t writer =
     fun context schema v writer ->
      match schema with
      | Any (module Any) ->
        (* FIXME: support any *)
        failf "any %s descriptor not supported when streaming" Any.name
      | Bool -> W.bool v writer
      | Coding { f } -> write_descriptor context (f None) v writer
      | Date -> (
        match W.date with
        | `Generic -> write_schema context date_tuple_schema v writer
        | `Custom f -> f v writer)
      | Fixpoint s -> write_descriptor context (Lazy.force s) v writer
      | Float -> W.float v writer
      | Ignore -> ()
      | Int -> W.int v writer
      | Int64 -> W.int64 v writer
      | Json -> (
        match W.json with
        | `Generic -> fail "fixme: schematic JSON streaming not implemented"
        | `Custom f -> f v writer)
      | List items_schema ->
        W.list_push (List.length v) writer;
        List.iter v ~f:(fun item ->
            write_schema context items_schema item writer);
        W.list_pop writer
      | Map { encode; descriptor; _ } ->
        write_descriptor context descriptor (encode v) writer
      | Null -> ()
      | Nullable schema -> (
        match v with
        | None -> W.option_none writer
        | Some v ->
          W.option_push writer;
          write_schema context schema v writer;
          W.option_pop writer)
      | Mapping items_schema ->
        write_schema context (mapping_schema items_schema) v writer
      | Object { fields; encode; _ } ->
        let rec write_fields :
            type t. t Hlist.t Schema.fields -> t Hlist.t writer =
         fun fields v writer ->
          match fields with
          | FieldEnd -> ()
          | Field { field = { name; schema; requirement; _ }; rest } ->
            let (head :: tail) = v in
            let () =
              match Schema.field_schema schema with
              | { descriptor = Ignore; _ } -> ()
              | schema -> (
                match requirement with
                | Default _ ->
                  W.object_field_push name writer;
                  write_schema context schema head writer;
                  W.object_field_pop writer
                | Optional -> (
                  match head with
                  | None -> W.object_field_opt_none name writer
                  | Some head ->
                    W.object_field_opt_push name writer;
                    write_schema context schema head writer;
                    W.object_field_opt_pop writer)
                | Required ->
                  W.object_field_push name writer;
                  write_schema context schema head writer;
                  W.object_field_pop writer
                | Computed | Computed_optional -> ())
            in
            write_fields rest tail writer
        in
        W.object_push writer;
        write_fields fields (encode v) writer;
        W.object_pop writer
      | Tuple { items; encode; decode = _ } ->
        let rec write_items : type t. t Schema.tuple -> t writer =
         fun items v writer ->
          match items with
          | ItemEnd -> ()
          | Item { schema; rest } ->
            let (head :: tail) = v in
            let () =
              W.tuple_item_push writer;
              write_schema context schema head writer;
              W.tuple_item_pop writer
            in
            write_items rest tail writer
        in
        W.tuple_push writer;
        write_items items (encode v) writer;
        W.tuple_pop writer
      | String -> W.string v writer
      | String_const _ -> ()
      | Time -> (
        match W.time with
        | `Generic -> W.float (Ptime.to_float_s v) writer
        | `Custom f -> f v writer)
      | Time_local -> (
        match W.time_local with
        | `Generic -> write_schema context time_local_pair_schema v writer
        | `Custom f -> f v writer)
      | Union { cases; _ } -> (
        let cases =
          let f i (Schema.Case { encoder; schema; _ } as case) value writer =
            match encoder value with
            | Some v ->
              W.union_push i case writer;
              write_schema context schema v writer;
              W.union_pop writer;
              Some ()
            | None -> None
          in
          List.mapi ~f cases
        in
        match List.find_map cases ~f:(fun case -> case v writer) with
        | Some res -> res
        | None ->
          failf "unmatched variant case in schema %a"
            Fmt.(option string)
            context.schema)
    and write_schema : type t. E.decoding -> t Schema.t -> t writer =
     fun context schema writer ->
      write_descriptor
        { context with schema = schema.id }
        schema.descriptor writer
    in
    let () =
      write_schema { schema = None; path = []; reason = "" } schema v writer
    in
    W.eof writer

  let to_channel schema value channel =
    let output = Impl.Output.of_channel channel in
    write schema value output

  let to_string schema value =
    let result = Buffer.create 4096 in
    let output = Impl.Output.of_buffer result in
    let _output = write schema value output in
    Buffer.contents result
end

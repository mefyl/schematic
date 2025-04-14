include Streaming_intf
module E = Error
open Base
open Schemas

type date_tuple = int * int * int [@@deriving schema]
type ('a, 'b) pair = 'a * 'b [@@deriving schema]
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

  let ok_or_fail = function
    | Result.Ok ok -> ok
    | Result.Error reason -> fail reason

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
        | List items_schema ->
          let _fixme = R.list_push reader in
          let items =
            let rec read_items acc reader =
              match R.list_item_push reader with
              | false -> acc
              | true ->
                let head = read_schema items_schema reader in
                let () = R.list_item_pop reader in
                read_items (head :: acc) reader
            in
            read_items [] reader |> List.rev
          in
          let () = R.list_pop reader in
          items
        | Sequence { item; construct; _ } -> (
          read_sequence item reader |> construct |> function
          | Result.Ok result -> result
          | Result.Error error -> failf "%s" error)
        | Map { decode; descriptor; _ } ->
          let value = read_descriptor descriptor reader in
          decode value |> ok_or_fail
        | Mapping { construct; item; _ } ->
          read_sequence (pair_schema Schemas.string_schema item) reader
          |> construct |> ok_or_fail
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
      and[@inline] read_sequence : type a. a Schema.t -> a Sequence.t reader =
       fun item reader ->
        let _fixme = R.list_push reader in
        let read_item reader =
          match R.list_item_push reader with
          | false ->
            let () = R.list_pop reader in
            None
          | true ->
            let head = read_schema item reader in
            let () = R.list_item_pop reader in
            Some (head, reader)
        in
        Base.Sequence.unfold ~init:reader ~f:read_item
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

  let write schema =
    let module W = Impl.Output in
    let rec write_descriptor :
        type t. E.decoding -> t Schema.descriptor -> t writer =
     fun context descriptor v writer ->
      match descriptor with
      | Any (module Any) ->
        (* FIXME: support any *)
        failf "any %s descriptor not supported when streaming" Any.name
      | Bool -> W.bool v writer
      | Coding { f } -> write_descriptor context (f None) v writer
      | Date -> (
        match W.date with
        | `Generic -> write_schema context date_tuple_schema v writer
        | `Custom f -> f v writer)
      | Fixpoint s ->
        (* Delay evaluation to break infinite recursions. FIXME: voids the whole
           optimization*)
        write_descriptor context (Lazy.force s) v writer
      | Float -> W.float v writer
      | Ignore -> ()
      | Int -> W.int v writer
      | Int64 -> W.int64 v writer
      | Json -> (
        match W.json with
        | `Generic -> fail "fixme: schematic JSON streaming not implemented"
        | `Custom f -> f v writer)
      | List items_schema ->
        let () = W.list_push writer in
        let () =
          List.iter v ~f:(fun v -> write_schema context items_schema v writer)
        in
        W.list_pop writer
      | Sequence { item; deconstruct; _ } ->
        let () = W.list_push writer in
        let () =
          deconstruct v
          |> Base.Sequence.iter ~f:(fun v -> write_schema context item v writer)
        in
        W.list_pop writer
      | Map { encode; descriptor; _ } ->
        write_descriptor context descriptor (encode v) writer
      | Null -> ()
      | Nullable s -> (
        match v with
        | None -> W.option_none writer
        | Some v ->
          let () = W.option_push writer in
          let () = write_schema context s v writer in
          W.option_pop writer)
      | Mapping { deconstruct; item; _ } ->
        let () = W.list_push writer in
        let () =
          v |> deconstruct
          |> Base.Sequence.iter ~f:(fun (key, data) ->
                 W.string key writer;
                 write_schema context item data writer)
        in
        W.list_pop writer
      | Object { fields; encode; _ } ->
        let () = W.object_push writer in
        let () =
          let rec write_fields :
              type t. t Hlist.t Schema.fields -> t Hlist.t -> unit =
           fun fields v ->
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
                    let () = W.object_field_push name writer in
                    let () = write_schema context schema head writer in
                    W.object_field_pop writer
                  | Optional -> (
                    match head with
                    | None -> W.object_field_opt_none name writer
                    | Some head ->
                      let () = W.object_field_opt_push name writer in
                      let () = write_schema context schema head writer in
                      W.object_field_opt_pop writer)
                  | Required ->
                    let () = W.object_field_push name writer in
                    let () = write_schema context schema head writer in
                    W.object_field_pop writer
                  | Computed | Computed_optional -> ())
              in
              write_fields rest tail
          in
          write_fields fields (encode v)
        in
        W.object_pop writer
      | Tuple { items; encode; decode = _ } ->
        let () = W.tuple_push writer in
        let () =
          let rec write_items : type t. t Schema.tuple -> t -> unit =
           fun items v ->
            match items with
            | ItemEnd -> ()
            | Item { schema; rest } ->
              let (head :: tail) = v in
              let () = write_schema context schema head writer in
              write_items rest tail
          in
          write_items items (encode v)
        in
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
      | Union { cases; _ } ->
        let rec search i = function
          | [] -> fail "no case match"
          | (Schema.Case { encoder; schema; _ } as case) :: tail -> (
            match encoder v with
            | Some v ->
              let () = W.union_push i case writer in
              let () = write_schema context schema v writer in
              W.union_pop writer
            | None -> search (i + 1) tail)
        in
        search 0 cases
    and write_schema : type t. E.decoding -> t Schema.t -> t writer =
     fun context schema ->
      write_descriptor { context with schema = schema.id } schema.descriptor
    in
    write_schema { schema = None; path = []; reason = "" } schema

  let to_channel schema value channel =
    let output = Impl.Output.of_channel channel in
    let () = write schema value output in
    Impl.Output.eof output

  let to_string schema value =
    let result = Buffer.create 4096 in
    let output = Impl.Output.of_buffer result in
    let _output = write schema value output in
    Buffer.contents result
end

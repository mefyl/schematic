include Streaming_intf
module E = Error
open Base
open Schemas

type date_tuple = int * int * int [@@deriving schema]
type 'a mapping = (string * 'a) list [@@deriving schema]
type time_local_pair = Ptime.t * int [@@deriving schema]

module Reader = struct
  module T = struct
    type ('a, 'state, 'error) t = 'state -> 'state * ('a, 'error) Result.t

    let return v state = (state, Result.Ok v)

    let bind v ~f =
      let res state =
        let state, v = v state in
        match v with
        | Result.Ok v -> f v state
        | Result.Error _ as error -> (state, error)
      in
      res

    let map = `Define_using_bind
  end

  include T
  include Base.Monad.Make3 (T)

  let ( let* ) = ( >>= )
  let ( let+ ) = ( >>| )
  let fail error reader = (reader, Result.Error error)
end

module Writer = struct
  type ('a, 'writer) t = 'a -> 'writer -> 'writer

  let ignore _ = Fn.id
  let using f t value writer = t (f value) writer

  let ( *> ) lhs rhs value writer =
    let writer = lhs writer in
    rhs value writer

  let ( <* ) lhs rhs value writer =
    let writer = lhs value writer in
    rhs writer

  let ( *:: ) lhs rhs value writer =
    let Hlist.(head :: tail) = value in
    let writer = lhs head writer in
    rhs tail writer

  let option some none value writer =
    match value with None -> none writer | Some value -> some value writer
end

module Make (Impl : IMPL) = struct
  let name = Impl.name

  module Input = Impl.Input
  module Output = Impl.Output

  type 'a reader = ('a, Impl.Input.t, E.decoding) Reader.t

  let requirement_return (type a req) (req : (a, req) Schema.requirement)
      (v : a) : req =
    match req with
    | Computed -> Some v
    | Computed_optional -> Some (Some v)
    | Default _ -> v
    | Optional -> Some v
    | Required -> v

  open Reader

  let unwrap_error context f reader =
    let reader, result = f reader in
    ( reader,
      Base.Result.map_error result ~f:(function
        | `Channel reason | `Content reason -> { context with E.reason }
        | `Decoding error -> error) )

  let read schema =
    let module R = Impl.Input in
    let rec read_descriptor :
        type t. E.decoding -> t Schema.descriptor -> t reader =
     fun context descriptor ->
      let err f = unwrap_error context f
      and fail fmt =
        let f reason = fail { context with reason } in
        Fmt.kstr f fmt
      and error_wrap (value, result) =
        (value, Result.map_error result ~f:(fun error -> `Decoding error))
      in
      match descriptor with
      | Any (module Any) ->
        fail "any %s descriptor not supported when streaming" Any.name
      | Bool -> R.bool |> err
      | Coding { f } -> read_descriptor context (f None)
      | Date -> (
        match R.date with
        | `Generic -> read_schema context date_tuple_schema
        | `Custom f -> f |> err)
      | Fixpoint s -> read_descriptor context (Lazy.force s)
      | Float -> R.float |> err
      | Ignore -> return ()
      | Int -> R.int |> err
      | Int64 -> R.int64 |> err
      | Json -> failwith "fixme: schematic JSON streaming not implemented"
      | List items_schema ->
        R.list (Fn.compose error_wrap (read_schema context items_schema)) |> err
      | Map { decode; descriptor; _ } -> (
        let* value = read_descriptor context descriptor in
        match decode value with
        | Result.Error reason -> fail "%s" reason
        | Result.Ok value -> return value)
      | Mapping items_schema ->
        read_schema context (mapping_schema items_schema)
      | Null -> return ()
      | Nullable schema ->
        R.option (Fn.compose error_wrap (read_schema context schema)) |> err
      | Object { fields = fields_schema; decode = decode_object; _ } ->
        let* () = R.object_push |> err in
        let* fields =
          let rec decode_object :
              type t.
              t Hlist.t Schema.fields ->
              R.t ->
              R.t * (t Hlist.t, E.decoding) Result.t = function
            | FieldEnd -> return Hlist.[]
            | Field { field; rest } -> (
              match Schema.field_schema field.schema with
              | { descriptor = Ignore; _ } ->
                let* rest = decode_object rest in
                return Hlist.(requirement_return field.requirement () :: rest)
              | schema ->
                let* head =
                  match field.requirement with
                  | Default _ | Optional | Required -> (
                    let* name = R.object_field_push field.name |> err in
                    (* FIXME: this assumes fields come in order *)
                    match name with
                    | Some name when String.(name = field.name) ->
                      let* head = read_schema context schema in
                      let* () = R.object_field_pop |> err in
                      return (requirement_return field.requirement head)
                    | _ -> (
                      match field.requirement with
                      | Computed -> (* unreachable *) return None
                      | Computed_optional -> (* unreachable *) return None
                      | Default value -> return value
                      | Optional -> return None
                      | Required -> fail "missing field: %s" field.name))
                  | Computed -> return None
                  | Computed_optional -> return None
                in
                let+ tail = decode_object rest in
                Hlist.(head :: tail))
          in
          decode_object fields_schema
        in
        let+ () = R.object_pop |> err in
        decode_object fields
      | String -> R.string |> err
      | String_const _ -> return ()
      | Time -> (
        match R.time with
        | `Generic -> (
          let* timestamp = read_descriptor context Float in
          match Ptime.of_float_s timestamp with
          | Some time -> return time
          | None -> fail "timestamp does not fit: %f" timestamp)
        | `Custom f -> f |> err)
      | Time_local -> (
        match R.time_local with
        | `Generic -> read_schema context time_local_pair_schema
        | `Custom f -> f |> err)
      | Tuple { items; decode = decode_tuple; _ } ->
        let* () = R.tuple_push |> err in
        let+ items =
          let rec decode_tuple :
              type t. t Schema.tuple -> (t, R.t, E.decoding) Reader.t = function
            | ItemEnd ->
              let+ () = R.tuple_pop |> err in
              Hlist.[]
            | Item { schema; rest } ->
              let* () = R.tuple_item_push |> err in
              let* head = read_schema context schema in
              let* () = R.tuple_item_pop |> err in
              let+ tail = decode_tuple rest in
              Hlist.(head :: tail)
          in
          decode_tuple items
        in
        decode_tuple items
      | Union { cases; _ } ->
        let* (Case case) = R.union_push cases |> err in
        let* value = read_schema context case.schema in
        let+ () = R.union_pop |> err in
        case.decoder value
    and read_schema : type t. E.decoding -> t Schema.t -> t reader =
     fun context schema v ->
      read_descriptor { context with schema = schema.id } schema.descriptor v
    in
    read_schema { schema = schema.Schema.id; path = []; reason = "" } schema

  let of_channel schema channel =
    let input = Impl.Input.of_channel channel in
    let (_ : Impl.Input.t), result = read schema input in
    result

  let of_string schema string =
    let read =
      let* result = read schema in
      let+ () =
        Impl.Input.eof
        |> unwrap_error { schema = schema.id; path = []; reason = "" }
      in
      result
    in
    let input = Impl.Input.of_string string in
    let (_ : Impl.Input.t), result = read input in
    result

  type 'a writer = ('a, Impl.Output.t) Writer.t

  open Writer

  let write schema =
    let module W = Impl.Output in
    let rec write_descriptor :
        type t. E.decoding -> t Schema.descriptor -> t writer =
     fun context -> function
      | Any (module Any) ->
        (* FIXME: support any *)
        Fmt.failwith "any %s descriptor not supported when streaming" Any.name
      | Bool -> W.bool
      | Coding { f } -> write_descriptor context (f None)
      | Date -> (
        match W.date with
        | `Generic -> write_schema context date_tuple_schema
        | `Custom f -> f)
      | Fixpoint s ->
        (* Delay evaluation to break infinite recursions. FIXME: voids the whole
           optimization*)
        fun v -> write_descriptor context (Lazy.force s) v
      | Float -> W.float
      | Ignore -> ignore
      | Int -> W.int
      | Int64 -> W.int64
      | Json -> (
        match W.json with
        | `Generic -> failwith "fixme: schematic JSON streaming not implemented"
        | `Custom writer -> writer)
      | List items_schema ->
        let items =
          let item = write_schema context items_schema in
          fun items writer ->
            List.fold items ~init:writer ~f:(fun writer v -> item v writer)
        in
        W.list_push *> items <* W.list_pop
      | Map { encode; descriptor; _ } ->
        using encode @@ write_descriptor context descriptor
      | Null -> ignore
      | Nullable s -> (
        let some = W.option_push *> write_schema context s <* W.option_pop in
        function None -> W.option_none | Some value -> some value)
      | Mapping items_schema ->
        write_schema context (mapping_schema items_schema)
      | Object { fields; encode; _ } ->
        let rec write_fields :
            type t. t Hlist.t Schema.fields -> t Hlist.t writer = function
          | FieldEnd -> ignore
          | Field { field = { name; schema; requirement; _ }; rest } -> (
            let rest = write_fields rest in
            match Schema.field_schema schema with
            | { descriptor = Ignore; _ } ->
              using (fun Hlist.(_ :: tail) -> tail) rest
            | schema -> (
              let field =
                W.object_field_push name *> write_schema context schema
                <* W.object_field_pop
              in
              match requirement with
              | Default _ -> field *:: rest
              | Optional -> option field Fn.id *:: rest
              | Required -> field *:: rest
              | Computed | Computed_optional -> ignore))
        in
        W.object_push *> (using encode @@ write_fields fields) <* W.object_pop
      | Tuple { items; encode; decode = _ } ->
        let rec write_items : type t. t Schema.tuple -> t writer = function
          | ItemEnd -> ignore
          | Item { schema; rest } ->
            let rest = write_items rest
            and item = write_schema context schema in
            item *:: rest
        in
        W.tuple_push *> (using encode @@ write_items items) <* W.tuple_pop
      | String -> W.string
      | String_const _ -> ignore
      | Time -> (
        match W.time with
        | `Generic -> using Ptime.to_float_s @@ W.float
        | `Custom f -> f)
      | Time_local -> (
        match W.time_local with
        | `Generic -> write_schema context time_local_pair_schema
        | `Custom f -> f)
      | Union { cases; _ } -> (
        let cases =
          let f i (Schema.Case { encoder; schema; _ } as case) =
            let write_case = write_schema context schema in
            fun value writer ->
              match encoder value with
              | Some v ->
                let writer =
                  (W.union_push i case *> write_case <* W.union_pop) v writer
                in
                Some writer
              | None -> None
          in
          List.mapi ~f cases
        in
        fun value writer ->
          match List.find_map cases ~f:(fun case -> case value writer) with
          | Some res -> res
          | None ->
            Fmt.failwith "unmatched variant case in schema %a"
              Fmt.(option string)
              context.schema)
    and write_schema : type t. E.decoding -> t Schema.t -> t writer =
     fun context schema ->
      write_descriptor { context with schema = schema.id } schema.descriptor
    in
    write_schema { schema = None; path = []; reason = "" } schema

  let to_channel schema value channel =
    let output = Impl.Output.of_channel channel in
    let _ouptut : Impl.Output.t = write schema value output in
    ()

  let to_string schema value =
    let result = Buffer.create 4096 in
    let output = Impl.Output.of_buffer result in
    let _output = write schema value output in
    Buffer.contents result
end

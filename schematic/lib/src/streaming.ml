include Streaming_intf
module E = Error
open Base
open Schemas

type date_tuple = int * int * int [@@deriving schema]
type ('a, 'b) pair = 'a * 'b [@@deriving schema]

let mapping_schema element_schema =
  list_schema @@ pair_schema string_schema element_schema

type time_local_pair = Ptime.t * int [@@deriving schema]

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

  type 'a reader = ('a, Impl.Input.t) Reader.t

  let requirement_return (type a req) (req : (a, req) Schema.requirement)
      (v : a) : req =
    match req with
    | Computed -> Some v
    | Computed_optional -> Some (Some v)
    | Default _ -> v
    | Optional -> Some v
    | Required -> v

  let fail context reason = raise (Content { context with E.Decoding.reason })
  let failf context fmt = Fmt.kstr (fail context) fmt

  let read schema reader =
    try
      let module R = Impl.Input in
      let rec read_descriptor :
          type t. E.decoding -> t Schema.descriptor -> t reader =
       fun context descriptor reader ->
        match descriptor with
        | Any (module Any) ->
          failf context "any %s descriptor not supported when streaming"
            Any.name
        | Bool -> R.bool reader
        | Coding { f } -> read_descriptor context (f None) reader
        | Date -> (
          match R.date with
          | `Generic -> read_schema context date_tuple_schema reader
          | `Custom f -> f reader)
        | Fixpoint s -> read_descriptor context (Lazy.force s) reader
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
                let head = read_schema context items_schema reader in
                let () = R.list_item_pop reader in
                read_items (head :: acc) reader
            in
            read_items [] reader |> List.rev
          in
          let () = R.list_pop reader in
          items
        | Map { decode; descriptor; _ } -> (
          let value = read_descriptor context descriptor reader in
          match decode value with
          | Result.Error error -> fail context error
          | Result.Ok value -> value)
        | Mapping items_schema ->
          read_schema context (mapping_schema items_schema) reader
        | Null -> ()
        | Nullable schema -> (
          match R.option_push reader with
          | false -> None
          | true ->
            let value = read_schema context schema reader in
            let () = R.option_pop reader in
            Some value)
        | Object { fields; decode = decode_object; _ } ->
          let[@inline] read_fields (type fields)
              (fields_schema : fields Hlist.t Schema_intf.fields) :
              fields Hlist.t =
            let open struct
              type ('has, 'want, 't) stack =
                | ( :: ) :
                    'head * ('has, 'head -> 'want, 't) stack
                    -> ('head -> 'has, 'want, 't) stack
                | [] : (unit, 't, 't) stack

              let rec rev :
                  type has want t.
                  want Hlist.t -> (has, want, t) stack -> t Hlist.t =
               fun acc -> function
                | [] -> acc
                | head :: tail -> rev (head :: acc) tail
            end in
            let rec read_field :
                type has t.
                string option ->
                (has, t, fields) stack ->
                t Hlist.t Schema.fields ->
                fields Hlist.t =
             fun current_field stack -> function
              | FieldEnd -> rev [] stack
              | Field { field; rest } -> (
                match Schema.field_schema field.schema with
                | { descriptor = Ignore; _ } ->
                  read_field current_field
                    (requirement_return field.requirement () :: stack)
                    rest
                | schema ->
                  let head, current_field =
                    match field.requirement with
                    | Default _ | Optional | Required -> (
                      let name =
                        match current_field with
                        | Some field -> Some field
                        | None -> (
                          match field.requirement with
                          | Optional ->
                            R.object_field_opt_push field.name reader
                          | _ -> R.object_field_push field.name reader)
                      in
                      (* FIXME: this assumes fields come in order *)
                      match name with
                      | Some name when String.(name = field.name) ->
                        let head =
                          read_schema
                            { context with path = name :: context.path }
                            schema reader
                        in
                        let () =
                          match field.requirement with
                          | Optional -> R.object_field_opt_pop reader
                          | _ -> R.object_field_pop reader
                        in
                        (requirement_return field.requirement head, None)
                      | _ -> (
                        match field.requirement with
                        | Computed -> (* unreachable *) (None, name)
                        | Computed_optional -> (* unreachable *) (None, name)
                        | Default value -> (value, name)
                        | Optional -> (None, name)
                        | Required ->
                          failf context "missing required field %S" field.name))
                    | Computed -> (None, current_field)
                    | Computed_optional -> (None, current_field)
                  in
                  read_field current_field (head :: stack) rest)
            in
            read_field None [] fields_schema
          in
          let () = R.object_push reader in
          let fields = read_fields fields in
          let () = R.object_pop reader in
          decode_object fields
        | String -> R.string reader
        | String_const _ -> ()
        | Time -> (
          match R.time with
          | `Generic -> (
            let timestamp = read_descriptor context Float reader in
            match Ptime.of_float_s timestamp with
            | Some time -> time
            | None -> failf context "timestamp does not fit: %f" timestamp)
          | `Custom f -> f reader)
        | Time_local -> (
          match R.time_local with
          | `Generic -> read_schema context time_local_pair_schema reader
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
                let head =
                  read_schema
                    { context with path = Int.to_string i :: context.path }
                    schema reader
                in
                let () = R.tuple_item_pop reader in
                let tail = decode_tuple (i + 1) rest in
                Hlist.(head :: tail)
            in
            decode_tuple 0 items
          in
          decode_tuple items
        | Union { cases; _ } ->
          let (Case case) = R.union_push cases reader in
          let value =
            read_schema
              { context with path = case.name :: context.path }
              case.schema reader
          in
          let () = R.union_pop reader in
          case.decoder value
      and read_schema : type t. E.decoding -> t Schema.t -> t reader =
       fun context schema input ->
        read_descriptor
          { context with schema = schema.id }
          schema.descriptor input
      in
      read_schema
        { schema = schema.Schema.id; path = []; reason = "" }
        schema reader
      |> Result.return
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

  open Writer

  let write schema =
    let module W = Impl.Output in
    let rec write_descriptor :
        type t. E.decoding -> t Schema.descriptor -> t writer =
     fun context -> function
      | Any (module Any) ->
        (* FIXME: support any *)
        failf context "any %s descriptor not supported when streaming" Any.name
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
        | `Generic ->
          fail context "fixme: schematic JSON streaming not implemented"
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
              | Optional ->
                let field_opt =
                  W.object_field_opt_push name *> write_schema context schema
                  <* W.object_field_opt_pop
                in
                option field_opt (W.object_field_opt_none name) *:: rest
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
            failf context "unmatched variant case in schema %a"
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

module E = Error
open Base

type date_tuple = int * int * int [@@deriving schema]
type ('a, 'b) pair = 'a * 'b [@@deriving schema]
type time_local_pair = Ptime.t * int [@@deriving schema]

let mapping_schema element_schema =
  Schemas.(list_schema @@ pair_schema string_schema element_schema)

let requirement_return (type a req) (req : (a, req) Schema.requirement) (v : a)
    : req =
  match req with
  | Computed -> Some v
  | Computed_optional -> Some (Some v)
  | Default _ -> v
  | Optional -> Some v
  | Required -> v

let buffer_length = 4096

let[@inline] round ?(header = 0) size =
  let rem =
    match Stdlib.Int.logand (size + header) 0x7 with 0 -> 0 | n -> 8 - n
  in
  (size + rem, rem)

let name = "marshal"
let fail reason = raise (Streaming.Content { E.Decoding.empty with reason })
let failf fmt = Fmt.kstr fail fmt

module Input = struct
  type t = {
    buffer : Bytes.t;
    fill : Bytes.t -> int -> int -> int;  (** [fill buffer position until] *)
    mutable position : int;
    mutable length : int;
  }

  let of_channel channel =
    let buffer = Bytes.create 4096 in
    let rec fill bytes position until =
      match Stdlib.input channel bytes position (until - position) with
      | 0 -> position
      | n ->
        let position = position + n in
        if position = until then position else fill bytes position until
    in
    let length = fill buffer 0 buffer_length in
    { buffer; position = 0; length; fill }

  let of_string input =
    let buffer = Bytes.unsafe_of_string_promise_no_mutation input in
    {
      buffer;
      position = 0;
      length = String.length input;
      fill = (fun _ position _ -> position);
    }

  let padding = Bytes.create 8

  let ensure t =
    if t.position = t.length then
      let () = t.position <- 0 in
      let () = t.length <- t.fill t.buffer 0 @@ Bytes.length t.buffer in
      if t.length < 8 then fail "premature end of file"

  let eof t =
    if t.position < t.length || t.fill t.buffer 0 @@ Bytes.length t.buffer > 0
    then
      failf {|expected end of input, got %c|} @@ Bytes.get t.buffer t.position

  let[@inline] int16 t =
    let () = ensure t in
    let () = t.position <- t.position + 2 in
    Bytes.unsafe_get_int16 t.buffer (t.position - 2)

  let[@inline] int64 t =
    let () = ensure t in
    let () = t.position <- t.position + 8 in
    Bytes.unsafe_get_int64 t.buffer (t.position - 8)

  let[@inline] int t = Int64.to_int_exn (int64 t)

  let string t =
    let string_length = int16 t in
    let input_length, padding_length = round ~header:2 string_length in
    let current = t.position in
    if current + string_length <= t.length then
      let () = t.position <- current + input_length in
      Bytes.To_string.sub ~pos:current ~len:string_length t.buffer
    else
      let content = Bytes.create string_length
      and prefix_length = t.length - current in
      let () =
        Bytes.blito ~src:t.buffer ~src_pos:current ~src_len:prefix_length
          ~dst:content ()
      in
      let () = t.position <- t.length
      and filled = t.fill content prefix_length string_length in
      let _ : int = t.fill padding 0 padding_length in
      if filled = string_length then
        Bytes.unsafe_to_string ~no_mutation_while_string_reachable:content
      else fail "end of input in string"

  let[@inline] bool t = int t <> 0
  let[@inline] float t = Int64.float_of_bits @@ int64 t

  let rec json t =
    match int t with
    | 78 -> `Null
    | 102 -> `Bool false
    | 116 -> `Bool true
    | 70 -> `Float (float t)
    | 83 -> `String (string t)
    | 65 ->
      let rec read acc = function
        | 0 -> `A (List.rev acc)
        | n ->
          let value = json t in
          read (value :: acc) (n - 1)
      in
      read [] @@ int t
    | 79 ->
      let rec read acc = function
        | 0 -> `O (List.rev acc)
        | n ->
          let name = string t
          and value = json t in
          read ((name, value) :: acc) (n - 1)
      in
      read [] @@ int t
    | tag -> failf "invalid JSON tag: %d" tag

  let read schema reader =
    try
      let rec read_descriptor : type a. a Schema.descriptor -> t -> a =
       fun descriptor reader ->
        match descriptor with
        | Any (module Any) ->
          failf "any %s descriptor not supported when streaming" Any.name
        | Bool -> bool reader
        | Coding { f } -> read_descriptor (f None) reader
        | Date -> read_schema date_tuple_schema reader
        | Fixpoint s -> read_descriptor (Lazy.force s) reader
        | Float -> float reader
        | Ignore -> ()
        | Int -> int reader
        | Int64 -> int64 reader
        | Json -> json reader
        | List items_schema ->
          let size = int reader in
          let rec read_items i acc reader =
            if i = 0 then List.rev acc
            else
              let head = read_schema items_schema reader in
              read_items (i - 1) (head :: acc) reader
          in
          read_items size [] reader
        | Map { decode; descriptor; _ } -> (
          let value = read_descriptor descriptor reader in
          match decode value with
          | Result.Error error -> fail error
          | Result.Ok value -> value)
        | Mapping items_schema ->
          read_schema (mapping_schema items_schema) reader
        | Null -> ()
        | Nullable schema ->
          if bool reader then Some (read_schema schema reader) else None
        | Object { fields = fields_schema; decode = decode_object; _ } ->
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
                        | Optional -> bool reader
                        | _ -> true
                      in
                      if some then
                        let head = read_schema schema reader in
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
          decode_object fields
        | String -> string reader
        | String_const _ -> ()
        | Time -> (
          let timestamp = read_descriptor Float reader in
          match Ptime.of_float_s timestamp with
          | Some time -> time
          | None -> failf "timestamp does not fit: %f" timestamp)
        | Time_local -> read_schema time_local_pair_schema reader
        | Tuple { items; decode = decode_tuple; _ } ->
          let items =
            let rec decode_tuple : type t. int -> t Schema.tuple -> t =
             fun i -> function
              | ItemEnd -> Hlist.[]
              | Item { schema; rest } ->
                let head = read_schema schema reader in
                let tail = decode_tuple (i + 1) rest in
                Hlist.(head :: tail)
            in
            decode_tuple 0 items
          in
          decode_tuple items
        | Union { cases; _ } -> (
          match List.nth cases @@ int reader with
          | None -> fail "case out of bounds"
          | Some (Case case) ->
            let value = read_schema case.schema reader in
            case.decoder value)
      and read_schema : type a. a Schema.t -> t -> a =
       fun schema input -> read_descriptor schema.descriptor input
      in
      read_schema schema reader |> Result.return
    with Streaming.Content error -> Result.Error error
end

let read = Input.read

let of_channel schema channel =
  let input = Input.of_channel channel in
  read schema input

let of_string schema string =
  let read reader =
    let ( let* ) = Result.( >>= ) in
    let* result = read schema reader in
    let* () =
      try Input.eof reader |> Result.return
      with _ ->
        Result.Error { E.schema = schema.Schema.id; path = []; reason = "" }
    in
    Result.Ok result
  in
  let input = Input.of_string string in
  let result = read input in
  result

module Output = struct
  type t = {
    buffer : Bytes.t;
    flush : Bytes.t -> int -> int -> unit;
    mutable position : int;
  }

  let of_flush flush =
    { buffer = Bytes.create buffer_length; flush; position = 0 }

  let of_channel channel = of_flush (Stdlib.output channel)

  let of_buffer buffer =
    of_flush (fun bytes pos len -> Buffer.add_subbytes buffer bytes ~pos ~len)

  let ensure size t =
    if t.position + size > buffer_length then
      let () = t.flush t.buffer 0 t.position in
      t.position <- 0

  let padding = Bytes.init 8 ~f:(fun _ -> '\x00')

  let[@inline] int16 v t =
    let () = ensure 2 t in
    let () = Bytes.unsafe_set_int16 t.buffer t.position v in
    t.position <- t.position + 2

  let[@inline] int64 v t =
    let () = ensure 8 t in
    let () = Bytes.unsafe_set_int64 t.buffer t.position v in
    t.position <- t.position + 8

  let[@inline] int v t = int64 (Int64.of_int v) t

  let string v t =
    let length = String.length v in
    let () = int16 length t in
    let size, pad = round ~header:2 length in
    let () = ensure size t in
    if size < buffer_length then
      let () =
        Bytes.blito
          ~src:(Bytes.unsafe_of_string_promise_no_mutation v)
          ~dst:t.buffer ~dst_pos:t.position ()
      in
      t.position <- t.position + size
    else
      let () =
        t.flush (Bytes.unsafe_of_string_promise_no_mutation v) 0 length
      in
      if pad > 0 then t.flush padding 0 pad

  let[@inline] bool v t = int (if v then 1 else 0) t
  let[@inline] float v t = int64 (Int64.bits_of_float v) t
  let eof t = if t.position > 0 then t.flush t.buffer 0 t.position

  (* FIXME: could be made more compact *)
  let json json t =
    let rec write = function
      | `Null -> int 78 t
      | `Bool false -> int 102 t
      | `Bool true -> int 116 t
      | `Float f ->
        let () = int 70 t in
        float f t
      | `String s ->
        let () = int 83 t in
        string s t
      | `A items ->
        let () = int 65 t in
        let () = int (List.length items) t in
        List.iter items ~f:write
      | `O fields ->
        let () = int 79 t in
        let () = int (List.length fields) t in
        List.iter fields ~f:(fun (name, value) ->
            let () = string name t in
            write value)
    in
    write json

  let write schema v writer =
    let rec write_descriptor : type a. a Schema.descriptor -> a -> t -> unit =
     fun schema v writer ->
      match schema with
      | Any (module Any) ->
        (* FIXME: support any *)
        failf "any %s descriptor not supported when streaming" Any.name
      | Bool -> bool v writer
      | Coding { f } -> write_descriptor (f None) v writer
      | Date -> write_schema date_tuple_schema v writer
      | Fixpoint s -> write_descriptor (Lazy.force s) v writer
      | Float -> float v writer
      | Ignore -> ()
      | Int -> int v writer
      | Int64 -> int64 v writer
      | Json -> json v writer
      | List items_schema ->
        int (List.length v) writer;
        List.iter v ~f:(fun item -> write_schema items_schema item writer)
      | Map { encode; descriptor; _ } ->
        write_descriptor descriptor (encode v) writer
      | Null -> ()
      | Nullable schema -> (
        match v with
        | None -> bool false writer
        | Some v ->
          bool true writer;
          write_schema schema v writer)
      | Mapping items_schema ->
        write_schema (mapping_schema items_schema) v writer
      | Object { fields; encode; _ } ->
        let rec write_fields :
            type a. a Hlist.t Schema.fields -> a Hlist.t -> t -> unit =
         fun fields v writer ->
          match fields with
          | FieldEnd -> ()
          | Field { field = { schema; requirement; _ }; rest } ->
            let (head :: tail) = v in
            let () =
              match Schema.field_schema schema with
              | { descriptor = Ignore; _ } -> ()
              | schema -> (
                match requirement with
                | Default _ -> write_schema schema head writer
                | Optional -> (
                  match head with
                  | None -> bool false writer
                  | Some head ->
                    let () = bool true writer in
                    write_schema schema head writer)
                | Required -> write_schema schema head writer
                | Computed | Computed_optional -> ())
            in
            write_fields rest tail writer
        in
        write_fields fields (encode v) writer
      | Tuple { items; encode; decode = _ } ->
        let rec write_items : type a. a Schema.tuple -> a -> t -> unit =
         fun items v writer ->
          match items with
          | ItemEnd -> ()
          | Item { schema; rest } ->
            let (head :: tail) = v in
            let () = write_schema schema head writer in
            write_items rest tail writer
        in
        write_items items (encode v) writer
      | String -> string v writer
      | String_const _ -> ()
      | Time -> write_descriptor Float (Ptime.to_float_s v) writer
      | Time_local -> write_schema time_local_pair_schema v writer
      | Union { cases; _ } ->
        let rec search i = function
          | [] -> fail "unmatched variant case"
          | Schema.Case { encoder; schema; _ } :: cases -> (
            match encoder v with
            | None -> search (i + 1) cases
            | Some v ->
              let () = int i writer in
              let () = write_schema schema v writer in
              ())
        in
        search 0 cases
    and write_schema : type a. a Schema.t -> a -> t -> unit =
     fun schema writer -> write_descriptor schema.descriptor writer
    in
    let () = write_schema schema v writer in
    eof writer
end

let write = Output.write

let to_channel schema value channel =
  let output = Output.of_channel channel in
  write schema value output

let to_string schema value =
  let result = Buffer.create 4096 in
  let output = Output.of_buffer result in
  let _output = write schema value output in
  Buffer.contents result

module E = Error
open Base

let buffer_length = 4096

let[@inline] round size =
  let rem = match Stdlib.Int.logand size 0x7 with 0 -> 0 | n -> 8 - n in
  (size + rem, rem)

module Impl = struct
  let name = "marshal"

  module Input = struct
    type t = {
      buffer : Bytes.t;
      fill : Bytes.t -> int -> int -> int;  (** [fill buffer position until] *)
      mutable position : int;
      mutable length : int;
    }

    let fail reason =
      (* FIXME: preserve context *)
      raise (Streaming.Content { E.Decoding.empty with reason })

    let failf fmt = Fmt.kstr fail fmt

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

    let ensure t =
      if t.position = t.length then
        let () = t.position <- 0 in
        let () = t.length <- t.fill t.buffer 0 @@ Bytes.length t.buffer in
        if t.length < 8 then fail "premature end of file"

    let eof t =
      if t.position < t.length || t.fill t.buffer 0 @@ Bytes.length t.buffer > 0
      then
        failf {|expected end of input, got %c|} @@ Bytes.get t.buffer t.position

    let int64 t =
      let () = ensure t in
      let () = t.position <- t.position + 8 in
      Bytes.unsafe_get_int64 t.buffer (t.position - 8)

    let int t = Int64.to_int_exn (int64 t)
    let padding = Bytes.create 8

    let string t =
      let string_length = int t in
      let input_length, padding_length = round string_length in
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

    let bool t = int t <> 0
    let date = `Generic
    let float t = Int64.float_of_bits @@ int64 t
    let json _ = failwith "not implemented"
    let list_push t = Some (int t)
    let list_pop _ = ()
    let list_item_push _ = true
    let list_item_pop _ = ()
    let object_push _ = ()
    let object_pop _ = ()
    let object_field_push _ = ()
    let object_field_pop _ = ()
    let object_field_opt_push t = bool t
    let object_field_opt_pop _ = ()
    let option_push t = bool t
    let option_pop _ = ()
    let time = `Generic
    let time_local = `Generic
    let tuple_push _ = ()
    let tuple_pop _ = ()
    let tuple_item_push _ = ()
    let tuple_item_pop _ = ()

    let union_push cases t =
      match List.nth cases @@ int t with
      | Some case -> case
      | None -> fail "case out of bounds"

    let union_pop _ = ()
  end

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

    let int64 v t =
      let () = ensure 8 t in
      let () = Bytes.unsafe_set_int64 t.buffer t.position v in
      t.position <- t.position + 8

    let int v t = int64 (Int64.of_int v) t
    let padding = Bytes.init 8 ~f:(fun _ -> '\x00')

    let string v t =
      let length = String.length v in
      let () = int length t in
      let size, pad = round length in
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

    let bool v t = int (if v then 1 else 0) t
    let date = `Generic
    let eof t = if t.position > 0 then t.flush t.buffer 0 t.position
    let float v t = int64 (Int64.bits_of_float v) t
    let json = `Generic
    let list_push size t = int size t
    let list_pop _ = ()
    let list_item_push _ = ()
    let list_item_pop _ = ()
    let object_push _ = ()
    let object_pop _ = ()
    let object_field_push _ _ = ()
    let object_field_pop _ = ()
    let object_field_opt_push _ t = bool true t
    let object_field_opt_pop _ = ()
    let object_field_opt_none _ t = bool false t
    let option_none t = bool false t
    let option_push t = bool true t
    let option_pop _ = ()
    let time = `Generic
    let time_local = `Generic
    let tuple_push _ = ()
    let tuple_pop _ = ()
    let tuple_item_push _ = ()
    let tuple_item_pop _ = ()
    let union_push idx _ t = int idx t
    let union_pop _ = ()
  end
end

include Streaming.Make (Impl)

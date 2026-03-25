open Base
include Json_intf

let schema = Schema.make Json
let schema_versioned (_ : Schema.version) = schema

let rec pp f json =
  let pp =
    let open Fmt in
    let indent = 2 in
    match json with
    | `Bool b -> const bool b
    | `Float f -> const float f
    | `String _ as value -> const string (Ezjsonm.value_to_string value)
    | `Null -> const string "null"
    | `A elements ->
      box
      @@ const string "["
         ++ (hvbox ~indent @@ (cut ++ const (list ~sep:comma pp) elements))
         ++ cut
         ++ const string "]"
    | `O elements ->
      box
      @@ const string "{"
         ++ (hvbox ~indent
            @@ cut
               ++ const
                    (list ~sep:comma
                       (box ~indent
                       @@ pair ~sep:(const string ":" ++ sp) Dump.string pp))
                    elements)
         ++ cut
         ++ const string "}"
  in
  pp f ()

module Impl : Coding.IMPL with type t = Ezjsonm.value = struct
  let coding = Coding

  type t = Ezjsonm.value

  let name = "JSON"
  let pp = pp
  let compare = Schema.compare schema

  include Coding.Defaults

  let type_name = function
    | `O _ -> "object"
    | `A _ -> "array"
    | `Bool _ -> "bool"
    | `Float _ -> "float"
    | `String _ -> "string"
    | `Null -> "null"

  let ( >>= ) = Result.( >>= )
  let ( >>| ) = Result.( >>| )
  let null () = `Null

  let of_any (type any) (coding : any Coding.t) (v : any) : t option =
    match coding with Coding -> Some v | _ -> None

  let of_bool b = `Bool b
  let of_fields f = `O f
  let of_float f = `Float f
  let of_int i = `Float (Float.of_int i)
  let of_int64 i = `Float (Float.of_int64 i)
  let of_json v = v
  let of_list l = `A l
  let of_option = function Some v -> v | None -> `Null
  let of_string s = `String s
  let of_time t = `String (Ptime.to_rfc3339 t)
  let of_time_local (t, tz_offset_s) = `String (Ptime.to_rfc3339 ~tz_offset_s t)
  let to_bool = function `Bool v -> Some v | _ -> None
  let to_fields = function `O fields -> Some fields | _ -> None
  let to_float = function `Float v -> Some v | _ -> None
  let to_int = function `Float v -> Some (Float.to_int v) | _ -> None
  let to_int64 = function `Float v -> Some (Float.to_int64 v) | _ -> None
  let to_json v = Some v
  let to_list = function `A l -> Some l | _ -> None
  let to_option = function `Null -> Some None | v -> Some (Some v)
  let to_string = function `String v -> Some v | _ -> None

  let to_time_ = function
    | `String s ->
      let error (`RFC3339 (_, e)) =
        Some (Stdlib.Format.asprintf "%a" Ptime.pp_rfc3339_error e)
      in
      Ptime.of_rfc3339 ~strict:true s |> Result.map_error ~f:error
    | _ -> Result.fail None

  let to_time v = to_time_ v >>| fun (t, _tz, _) -> t

  let to_time_local v =
    to_time_ v >>= function
    | t, Some tz, _ -> Result.return (t, tz)
    | _, None, _ -> Result.fail (Some "missing timezone")
end

module Make (Json_impl : Impl) = struct
  include Coding.Make (Impl)

  let content_type = "application/json"
  let of_string s = Json_impl.of_string s
  let from_channel c = of_string (In_channel.input_all c)
  let to_channel chan t = Ezjsonm.value_to_channel ~minify:true chan t
  let to_string t = Json_impl.to_string t
  let pp = pp
  let to_ezjsonm v = v

  let rec compare l r =
    match (l, r) with
    | `Null, `Null -> 0
    | `Null, _ -> -1
    | _, `Null -> 1
    | `Bool l, `Bool r -> Bool.compare l r
    | `Bool _, _ -> -1
    | _, `Bool _ -> 1
    | `Float l, `Float r -> Float.compare l r
    | `Float _, _ -> -1
    | _, `Float _ -> 1
    | `String l, `String r -> String.compare l r
    | `String _, _ -> -1
    | _, `String _ -> 1
    | `A l, `A r -> List.compare compare l r
    | `A _, _ -> -1
    | _, `A _ -> 1
    | `O l, `O r ->
      let compare (l, _) (r, _) = String.compare l r
      and compare_field (lk, lv) (rk, rv) =
        match String.compare lk rk with 0 -> compare lv rv | n -> n
      in
      let l = List.sort ~compare l
      and r = List.sort ~compare r in
      List.compare compare_field l r

  include Comparable.Make (struct
    type t = Ezjsonm.value

    let compare = compare
    let sexp_of_t = Ezjsonm.sexp_of_value
  end)
end

module Impl_ezjsonm = struct
  let of_string s =
    let decoder = Json_scanner.of_string s
    and parse scanner =
      let exception Abort of string in
      let module Stack = struct
        type t =
          | In_array of Ezjsonm.value list * t
          | In_object of string * (string * Ezjsonm.value) list * t
          | Empty
      end in
      let open Json_scanner in
      let dec () = scan scanner in
      let rec value l stack =
        match l with
        | Object_start -> obj [] stack
        | Array_start -> arr [] stack
        | Null -> continue `Null stack
        | Bool b -> continue (`Bool b) stack
        | Float f -> continue (`Float f) stack
        | String s -> continue (`String s) stack
        | token -> raise (Abort (Fmt.str "unexpected %a" pp_token token))
      and arr so_far stack =
        match dec () with
        | Array_end -> continue (`A (List.rev so_far)) stack
        | l ->
          let stack = Stack.In_array (so_far, stack) in
          value l stack
      and obj so_far stack =
        match dec () with
        | Object_end -> continue (`O (List.rev so_far)) stack
        | Name n ->
          let stack = Stack.In_object (n, so_far, stack) in
          value (dec ()) stack
        | token -> raise (Abort (Fmt.str "unexpected %a" pp_token token))
      and continue v stack =
        match stack with
        | Stack.In_array (vs, stack) ->
          let so_far = v :: vs in
          arr so_far stack
        | Stack.In_object (n, ms, stack) ->
          let so_far = (n, v) :: ms in
          obj so_far stack
        | Stack.Empty -> (
          match dec () with
          | End -> v
          | token -> raise (Abort (Fmt.str "unexpected %a" pp_token token)))
      in
      match value (dec ()) Empty with
      | value -> Result.Ok value
      | exception Abort error -> Error error
      | exception Json_scanner.Error.Parse_error error ->
        Error (Fmt.str "%a" Json_scanner.Error.pp error)
    in
    parse decoder

  let to_string t = Ezjsonm.value_to_string ~minify:true t
end

module T = Make (Impl_ezjsonm)
include T
module String = Coding.Make_string (T)

module Streaming_impl = struct
  type t = Ezjsonm.value

  let coding = Coding
  let name = Impl.name

  module Input = struct
    type t = unit

    let of_channel _ = failwith "JSON streaming deserialization not implemented"
    let of_string _ = failwith "JSON streaming deserialization not implemented"
    let bool _ = failwith "JSON streaming deserialization not implemented"

    let date =
      `Custom
        (fun _ -> failwith "JSON streaming deserialization not implemented")

    let eof _ = failwith "JSON streaming deserialization not implemented"
    let float _ = failwith "JSON streaming deserialization not implemented"
    let int _ = failwith "JSON streaming deserialization not implemented"
    let int64 _ = failwith "JSON streaming deserialization not implemented"
    let json _ = failwith "JSON streaming deserialization not implemented"
    let list_push _ = failwith "JSON streaming deserialization not implemented"
    let list_pop _ = failwith "JSON streaming deserialization not implemented"

    let list_item_push _ =
      failwith "JSON streaming deserialization not implemented"

    let list_item_pop _ =
      failwith "JSON streaming deserialization not implemented"

    let object_push _ =
      failwith "JSON streaming deserialization not implemented"

    let object_pop _ = failwith "JSON streaming deserialization not implemented"

    let object_field_push _ =
      failwith "JSON streaming deserialization not implemented"

    let object_field_pop _ =
      failwith "JSON streaming deserialization not implemented"

    let object_field_opt_push _ =
      failwith "JSON streaming deserialization not implemented"

    let object_field_opt_pop _ =
      failwith "JSON streaming deserialization not implemented"

    let option_push _ =
      failwith "JSON streaming deserialization not implemented"

    let option_pop _ = failwith "JSON streaming deserialization not implemented"
    let string _ = failwith "JSON streaming deserialization not implemented"

    let time =
      `Custom
        (fun _ -> failwith "JSON streaming deserialization not implemented")

    let time_local =
      `Custom
        (fun _ -> failwith "JSON streaming deserialization not implemented")

    let tuple_push _ = failwith "JSON streaming deserialization not implemented"
    let tuple_pop _ = failwith "JSON streaming deserialization not implemented"

    let tuple_item_push _ =
      failwith "JSON streaming deserialization not implemented"

    let tuple_item_pop _ =
      failwith "JSON streaming deserialization not implemented"

    let union_push _ _ =
      failwith "JSON streaming deserialization not implemented"

    let union_pop _ = failwith "JSON streaming deserialization not implemented"
  end

  module Output = struct
    type t = {
      buffer : Bytes.t;
      encoder : Jsonm.encoder;
      writer : Bytesrw.Bytes.Writer.t;
    }

    let of_bytes_writer writer =
      let output =
        {
          buffer = Bytes.create (Bytesrw.Bytes.Writer.slice_length writer);
          encoder = Jsonm.encoder `Manual;
          writer;
        }
      in
      let () =
        Jsonm.Manual.dst output.encoder output.buffer 0
          (Bytes.length output.buffer)
      in
      output

    let flush output =
      let length = Bytes.length output.buffer in
      let () =
        Bytesrw.Bytes.Writer.write output.writer
        @@ Bytesrw.Bytes.Slice.make output.buffer ~first:0
             ~length:(length - Jsonm.Manual.dst_rem output.encoder)
      in
      Jsonm.Manual.dst output.encoder output.buffer 0 length

    let put' output k =
      let rec encode t =
        match Jsonm.encode output.encoder t with
        | `Ok -> ()
        | `Partial ->
          let () = flush output in
          encode `Await
      in
      encode k

    let put output lexeme = put' output (`Lexeme lexeme)
    let bool bool output = put output (`Bool bool)
    let string string output = put output (`String string)
    let date = `Generic

    let eof output =
      let () = put' output `End in
      Bytesrw.Bytes.Writer.write_eod output.writer

    let float float output = put output (`Float float)
    let int int output = float (Float.of_int int) output
    let int64 int64 output = float (Float.of_int64 int64) output
    let list_push output = put output `As
    let list_pop output = put output `Ae
    let list_item_push _ = ()
    let list_item_pop _ = ()
    let object_push output = put output `Os
    let object_pop output = put output `Oe
    let object_field_push name output = put output (`Name name)
    let object_field_pop _ = ()
    let object_field_opt_push = object_field_push
    let object_field_opt_pop = object_field_pop
    let object_field_opt_none _ _ = ()
    let option_none output = put output `Null

    let json =
      let rec json v t =
        match v with
        | `Null -> option_none t
        | `Bool b -> bool b t
        | `Float f -> float f t
        | `String s -> string s t
        | `A items ->
          list_push t;
          List.iter items ~f:(fun v ->
              list_item_push t;
              json v t;
              list_item_pop t);
          list_pop t
        | `O fields ->
          object_push t;
          List.iter fields ~f:(fun (name, v) ->
              object_field_push name t;
              json v t;
              object_field_pop t);
          object_pop t
      in
      `Custom json

    let option_push _ = ()
    let option_pop _ = ()
    let time = `Custom (fun t writer -> string (Ptime.to_rfc3339 t) writer)

    let time_local =
      `Custom
        (fun (t, tz_offset_s) writer ->
          string (Ptime.to_rfc3339 ~tz_offset_s t) writer)

    let tuple_push = list_push
    let tuple_pop = list_pop
    let tuple_item_push = list_item_push
    let tuple_item_pop = list_item_pop
  end
end

module Streaming = Streaming.Make (Streaming_impl)

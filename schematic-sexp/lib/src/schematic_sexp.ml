open Base
open Schematic
module Sexp = Sexplib.Sexp
include Sexp
include Schematic_sexp_intf

type _ Coding.coding += Coding : t Coding.coding
type Coding.any += Sexp of t

module Json_conv (Sexp : sig
  type t =
    | Atom of string
    | List of t list
end) =
struct
  let rec of_json = function
    | `Null -> Sexp.Atom ":null"
    | `Bool b -> List [ Atom ":b"; Atom (Bool.to_string b) ]
    | `Float f -> List [ Atom ":f"; Atom (Float.to_string f) ]
    | `String s -> List [ Atom ":s"; Atom s ]
    | `A items -> List [ Atom ":a"; List (List.map ~f:of_json items) ]
    | `O fields ->
      List
        [
          Atom ":o";
          List
            (List.map
               ~f:(fun (name, value) -> Sexp.List [ Atom name; of_json value ])
               fields);
        ]

  (* Shamelessly stolen from json-derivers, without the dependency on an
     obsolete version of Base *)
  let to_json json =
    let rec to_json = function
      | Sexp.Atom ":null" -> `Null
      | List [ Atom ":b"; Atom b ] -> `Bool (Bool.of_string b)
      | List [ Atom ":f"; Atom f ] -> `Float (Float.of_string f)
      | List [ Atom ":s"; Atom s ] -> `String s
      | List [ Atom ":a"; List items ] -> `A (List.map ~f:to_json items)
      | List [ Atom ":o"; List fields ] -> `O (List.map fields ~f:field_to_json)
      | _ -> failwith "invalid sexp shape for JSON"
    and field_to_json = function
      | List [ Atom name; value ] -> (name, to_json value)
      | _ -> failwith "invalid sexp shape for JSON"
    in
    try Some (to_json json) with _ -> None
end

module Coding : Coding.IMPL with type t = t = struct
  let coding = Coding

  include Json_conv (Sexp)

  type nonrec t = t

  let name = "s-expression"
  let compare = Sexplib.Sexp.compare
  let pp = Sexplib.Sexp.pp

  include Coding.Defaults

  let type_name = function
    | Sexp.Atom _ -> "incompatible atom"
    | Sexp.List [] -> "empty list"
    | Sexp.List _ -> "incompatible list"

  (* Helpers *)
  let of_atom conv = function
    | Sexp.Atom v -> ( try Some (conv v) with _ -> None)
    | _ -> None

  let to_atom conv v = Sexp.Atom (conv v)

  (* Interface *)
  let null () = Sexp.unit
  let of_any = function Sexp v -> Some v | _ -> None
  let of_bool b = Sexp.Atom (Bool.to_string b)

  let of_fields fields =
    Sexp.List
      (List.map ~f:(fun (name, v) -> Sexp.(List [ Atom name; v ])) fields)

  let of_float = to_atom Float.to_string
  let of_int = to_atom Int.to_string
  let of_int64 = to_atom Int64.to_string
  let of_list l = Sexp.List l
  let of_option = function Some v -> Sexp.List [ v ] | None -> Sexp.List []
  let of_string s = Sexp.Atom s
  let of_time t = Sexp.Atom (Ptime.to_rfc3339 t)

  let of_time_local (t, tz_offset_s) =
    Sexp.Atom (Ptime.to_rfc3339 ~tz_offset_s t)

  let to_any = function v -> Sexp v
  let to_bool = of_atom Bool.of_string

  let to_fields = function
    | Sexp.List fields ->
      List.map fields ~f:(function
        | Sexp.(List [ Atom name; value ]) -> Some (name, value)
        | _ -> None)
      |> Option.all
    | _ -> None

  let to_float = of_atom Float.of_string
  let to_int = of_atom Int.of_string
  let to_int64 = of_atom Int64.of_string
  let to_list = function Sexp.List l -> Some l | _ -> None

  let to_option = function
    | Sexp.List [ v ] -> Some (Some v)
    | Sexp.List [] -> Some None
    | _ -> None

  let to_string = function Sexp.Atom s -> Some s | _ -> None

  let to_time_ = function
    | Sexp.Atom s ->
      let error (`RFC3339 (_, e)) =
        Some (Stdlib.Format.asprintf "%a" Ptime.pp_rfc3339_error e)
      in
      Ptime.of_rfc3339 ~strict:true s |> Result.map_error ~f:error
    | _ -> Result.fail None

  let to_time v =
    let open Result in
    to_time_ v >>| fun (t, _tz, _) -> t

  let to_time_local v =
    let open Result in
    to_time_ v >>= function
    | t, Some tz, _ -> Result.return (t, tz)
    | _, None, _ -> Result.fail (Some "missing timezone")
end

module Streaming = struct
  let name = Coding.name

  include Json_conv (Csexp)

  module Input = struct
    type t = {
      buffer : Bytes.t;
      mutable length : int;
      mutable position : int;
      channel : Stdlib.in_channel option;
    }

    let fail reason =
      (* FIXME: preserve context *)
      raise
        (Schematic.Streaming.Content
           { Schematic.Error.Decoding.empty with reason })

    let failf fmt = Fmt.kstr fail fmt

    let of_channel channel =
      let buffer = Bytes.create 4096 in
      { buffer; position = 0; length = 0; channel = Some channel }

    let of_string input =
      let buffer = Bytes.unsafe_of_string_promise_no_mutation input in
      { buffer; position = 0; length = String.length input; channel = None }

    let ensure t =
      if t.position < t.length then true
      else
        match t.channel with
        | None -> false
        | Some channel ->
          let length = Stdlib.input channel t.buffer 0 4096 in
          if length > 0 then
            let () = t.position <- 0
            and () = t.length <- length in
            true
          else false

    let eof t =
      if ensure t then
        failf {|expected end of input, got %c|} @@ Bytes.get t.buffer t.position

    let par_l t =
      if ensure t then
        match Bytes.get t.buffer t.position with
        | '(' -> t.position <- t.position + 1
        | c -> failf {|expected "(", got %c|} c
      else failf {|expected "(", got end of input|}

    let[@inline] par_r_maybe t =
      if ensure t then
        match Bytes.get t.buffer t.position with ')' -> true | _ -> false
      else failf {|expected ")", got end of input|}

    let par_r t = if par_r_maybe t then t.position <- t.position + 1

    let string t =
      let atom_length =
        let rec read_int n =
          if ensure t then
            let () = t.position <- t.position + 1 in
            match Bytes.get t.buffer (t.position - 1) with
            | ':' -> n
            | '0' -> read_int ((n * 10) + 0)
            | '1' -> read_int ((n * 10) + 1)
            | '2' -> read_int ((n * 10) + 2)
            | '3' -> read_int ((n * 10) + 3)
            | '4' -> read_int ((n * 10) + 4)
            | '5' -> read_int ((n * 10) + 5)
            | '6' -> read_int ((n * 10) + 6)
            | '7' -> read_int ((n * 10) + 7)
            | '8' -> read_int ((n * 10) + 8)
            | '9' -> read_int ((n * 10) + 9)
            | c -> failf {|invalid char in atom length: %c|} c
          else failf {|end of input in atom size|}
        in
        read_int 0
      in
      let current = t.position in
      if current + atom_length <= t.length then
        let () = t.position <- current + atom_length in
        Bytes.To_string.sub ~pos:current ~len:atom_length t.buffer
      else
        match t.channel with
        | None -> fail {|end of input in atom|}
        | Some channel ->
          let length = t.length in
          let () = t.length <- 0
          and suffix =
            Stdlib.really_input_string channel (atom_length - length + current)
          in
          if current = length then suffix
          else
            Bytes.To_string.sub ~pos:current ~len:(length - current) t.buffer
            ^ suffix

    let bool reader =
      match string reader with
      | "true" -> true
      | "false" -> false
      | string -> failf {|expected \"true\" or \"false\", got %S|} string

    let date = `Generic

    let[@inline] decoded_string name decode reader =
      let string = string reader in
      match decode string with
      | Some value -> value
      | None -> failf {|expected %s, got %S|} name string

    let float t = decoded_string "float" Float.of_string_opt t
    let int t = decoded_string "integer" Int.of_string_opt t
    let int64 t = decoded_string "integer" Int64.of_string_opt t

    let list_of_sexp f = function
      | Csexp.List l -> List.map ~f l
      | _ -> failwith "ffs"

    (* FIXME: stream *)
    let json t =
      let rec get_sexp =
        let open Csexp.Parser.Stack in
        function
        | Sexp (sexp, Empty) -> sexp
        | stack ->
          if ensure t then
            match Bytes.get t.buffer t.position with
            | '(' ->
              let () = t.position <- t.position + 1 in
              get_sexp @@ add_token Lparen stack
            | ')' ->
              let () = t.position <- t.position + 1 in
              get_sexp @@ add_token Rparen stack
            | _ -> get_sexp @@ add_atom (string t) stack
          else fail "end of input while parsing inner sexp"
      in
      let sexp = get_sexp Empty in
      match to_json sexp with
      | Some json -> json
      | None -> fail "invalid sexp shape for JSON"

    let list_push reader = par_l reader
    let list_pop = par_r
    let list_item_push reader = not @@ par_r_maybe reader
    let list_item_pop _ = ()
    let object_push = par_l
    let object_pop = par_r
    let object_field_push _ = ()
    let object_field_pop _ = ()

    let object_field_opt_push t =
      let () = par_l t in
      if par_r_maybe t then
        let () = t.position <- t.position + 1 in
        false
      else true

    let object_field_opt_pop = par_r

    let option_push t =
      let () = par_l t in
      if par_r_maybe t then
        let () = t.position <- t.position + 1 in
        false
      else true

    let option_pop = par_r
    let time = `Generic
    let time_local = `Generic
    let tuple_push = par_l
    let tuple_pop = par_r
    let tuple_item_push _ = ()
    let tuple_item_pop _ = ()

    let union_push cases reader =
      let () = par_l reader in
      let string = string reader in
      match
        List.find cases ~f:(fun (Schematic.Schema.Case { name; _ }) ->
            String.(name = string))
      with
      | None -> failf "no case matches %S" string
      | Some case -> case

    let union_pop = par_r
  end

  module Output = struct
    type t = {
      put : string -> unit;
      space_needed : bool;
    }

    let of_put put = { put; space_needed = false }
    let of_channel channel = of_put (Stdlib.output_string channel)
    let of_buffer buffer = of_put (Buffer.add_string buffer)

    let ( let* ) v f writer =
      let writer = v writer in
      f () writer

    let string string t =
      t.put @@ Int.to_string @@ String.length string;
      t.put ":";
      t.put string

    let par_l t = t.put "("
    let par_r t = t.put ")"
    let encoded_string encode v = string (encode v)
    let bool v = string (if v then "true" else "false")
    let date = `Generic
    let eof _ = ()
    let float = encoded_string Float.to_string
    let int = encoded_string Int.to_string
    let int64 = encoded_string Int64.to_string

    let json =
      (* FIXME: stream *)
      let json sexp writer =
        let sexp = of_json sexp in
        writer.put @@ Csexp.to_string sexp
      in
      `Custom json

    let list_push = par_l
    let list_pop = par_r
    let list_item_push _ = ()
    let list_item_pop _ = ()
    let object_push = par_l
    let object_pop = par_r
    let object_field_push _ _ = ()
    let object_field_pop _ = ()
    let object_field_opt_push _ t = par_l t
    let object_field_opt_pop t = par_r t
    let object_field_opt_none _ t = t.put "()"
    let option_none t = t.put "()"
    let option_push = par_l
    let option_pop = par_r
    let time = `Generic
    let time_local = `Generic
    let tuple_push = par_l
    let tuple_pop = par_r
    let tuple_item_push _ = ()
    let tuple_item_pop _ = ()

    let union_push _idx (Schematic.Schema.Case { name; _ }) t =
      let () = par_l t in
      string name t

    let union_pop = par_r
  end
end

include Schematic.Coding.Make (Coding)
include Schematic.Coding.Any (Coding)
module Stream = Schematic.Streaming.Make (Streaming)

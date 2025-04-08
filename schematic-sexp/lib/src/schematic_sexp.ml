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
    type token = Sexplib.Parser.token

    let pp_token f = function
      | Sexplib.Parser.STRING s -> Fmt.Dump.string f s
      | LPAREN -> Fmt.string f "("
      | RPAREN -> Fmt.string f ")"
      | EOF -> Fmt.string f "EOF"
      | HASH_SEMI -> Fmt.string f "#;"

    type t = {
      buffer : Buffer.t;
      get_char : unit -> char;
      get_string : int -> string;
      input : Csexp.Parser.Lexer.t;
      mutable token : token option;
    }

    let fail reason =
      (* FIXME: preserve context *)
      raise
        (Schematic.Streaming.Content
           { Schematic.Error.Decoding.empty with reason })

    let failf fmt = Fmt.kstr fail fmt

    let of_lexbuf get_char get_string =
      {
        buffer = Buffer.create 4096;
        get_char;
        get_string;
        input = Csexp.Parser.Lexer.create ();
        token = None;
      }

    let of_channel channel =
      let buffer = Bytes.create 4096
      and pos = ref 0
      and length = ref 0 in
      let refill () =
        match Stdlib.input channel buffer 0 (Bytes.length buffer) with
        | 0 -> raise End_of_file
        | l ->
          let () = length := l
          and () = pos := 0 in
          ()
      in
      let rec get_char () =
        let current = !pos in
        if current < !length then
          let () = pos := current + 1 in
          Bytes.get buffer current
        else
          let () = refill () in
          get_char ()
      and get_string n =
        let current = !pos in
        if current + n <= !length then
          let () = pos := current + n in
          Bytes.To_string.sub ~pos:current ~len:n buffer
        else
          let l = !length in
          let () = length := 0 in
          let suffix = Stdlib.really_input_string channel (n - l + current) in
          if current = l then suffix
          else
            Bytes.To_string.sub ~pos:current ~len:(l - current) buffer ^ suffix
      in
      of_lexbuf get_char get_string

    let of_string input =
      let len = String.length input
      and pos = ref 0 in
      let get_char () =
        if !pos < len then
          let () = pos := !pos + 1 in
          input.[!pos - 1]
        else raise End_of_file
      and get_string n =
        let start = !pos in
        if start + n <= len then
          let () = pos := start + n in
          String.sub ~pos:start ~len:n input
        else raise End_of_file
      in
      of_lexbuf get_char get_string

    let token = function
      | { token = Some token; _ } as t ->
        let () = t.token <- None in
        token
      | { input; get_char; get_string; _ } ->
        let rec get () =
          match Csexp.Parser.Lexer.feed input @@ get_char () with
          | Await -> get ()
          | Lparen -> Sexplib.Parser.LPAREN
          | Rparen -> RPAREN
          | Atom n -> STRING (get_string n)
          | exception Csexp.Parser.Parse_error reason -> fail reason
        in
        let res = try get () with End_of_file -> EOF in
        res

    let push_back token reader =
      let () = assert (Option.is_none reader.token) in
      reader.token <- Some token

    let eof reader =
      match token reader with
      | EOF -> ()
      | token -> failf {|expected end of input, got %a|} pp_token token

    let par_l reader =
      match token reader with
      | LPAREN -> ()
      | token -> failf {|expected "(", got %a|} pp_token token

    let par_r reader =
      match token reader with
      | RPAREN -> ()
      | token -> failf {|expected ")", got %a|} pp_token token

    let par_r_maybe reader =
      match token reader with
      | RPAREN -> true
      | token ->
        let () = push_back token reader in
        false

    let string reader =
      match token reader with
      | STRING v -> v
      | token -> failf {|expected string, got %a|} pp_token token

    let bool reader =
      match string reader with
      | "true" -> true
      | "false" -> false
      | string -> failf {|expected \"true\" or \"false\", got %S|} string

    let date = `Generic

    let decoded_string name decode reader =
      let string = string reader in
      match decode string with
      | Some value -> value
      | None -> failf {|expected %s, got %S|} name string

    let float = decoded_string "float" Float.of_string_opt
    let int = decoded_string "integer" Int.of_string_opt
    let int64 = decoded_string "integer" Int64.of_string_opt

    let list_of_sexp f = function
      | Csexp.List l -> List.map ~f l
      | _ -> failwith "ffs"

    let json =
      (* FIXME: stream, or even better implement and use the generic
         implementation *)
      let f reader =
        let rec get_sexp =
          let open Csexp.Parser.Stack in
          function
          | Sexp (sexp, Empty) -> sexp
          | stack ->
            let stack =
              match token reader with
              | LPAREN -> add_token Lparen stack
              | RPAREN -> add_token Rparen stack
              | HASH_SEMI -> stack
              | STRING atom -> add_atom atom stack
              | EOF -> fail "end of input while parsing inner sexp"
            in
            get_sexp stack
        in
        let sexp = get_sexp Empty in
        match to_json sexp with
        | Some yes -> yes
        | None -> fail "invalid sexp shape for JSON"
      in
      `Custom f

    let list_push reader =
      let () = par_l reader in
      None

    let list_pop = par_r

    let list_item_push reader =
      match par_r_maybe reader with
      | true ->
        let () = push_back RPAREN reader in
        false
      | false -> true

    let list_item_pop _ = ()
    let object_push = par_l
    let object_pop = par_r

    let object_field_push _ reader =
      match par_r_maybe reader with
      | true ->
        let () = push_back RPAREN reader in
        None
      | false ->
        let () = par_l reader in
        let string = string reader in
        Some string

    let object_field_pop = par_r

    let option_push reader =
      let () = par_l reader in
      not (par_r_maybe reader)

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
    let return = Fn.id

    let ( let* ) v f writer =
      let writer = v writer in
      f () writer

    let string string t =
      t.put @@ Int.to_string @@ String.length string;
      t.put ":";
      t.put string;
      t

    let par_l t =
      t.put "(";
      t

    let par_r t =
      t.put ")";
      t

    let encoded_string encode v = string (encode v)
    let bool v = string (if v then "true" else "false")
    let date = `Generic
    let eof = return
    let float = encoded_string Float.to_string
    let int = encoded_string Int.to_string
    let int64 = encoded_string Int64.to_string

    let json =
      (* FIXME: stream *)
      let f sexp writer =
        let sexp = of_json sexp in
        let () = writer.put @@ Csexp.to_string sexp in
        writer
      in
      `Custom f

    let list_push = par_l
    let list_pop = par_r
    let list_item_push = return
    let list_item_pop = return
    let object_push = par_l
    let object_pop = par_r

    let object_field_push name writer =
      let _ = par_l writer in
      string name writer

    let object_field_pop = par_r

    let option_none writer =
      writer.put "()";
      writer

    let option_push = par_l
    let option_pop = par_r
    let time = `Generic
    let time_local = `Generic
    let tuple_push = par_l
    let tuple_pop = par_r
    let tuple_item_push = return
    let tuple_item_pop = return

    let union_push _idx (Schematic.Schema.Case { name; _ }) =
      object_field_push name

    let union_pop = par_r
  end
end

include Schematic.Coding.Make (Coding)
include Schematic.Coding.Any (Coding)
module Stream = Schematic.Streaming.Make (Streaming)

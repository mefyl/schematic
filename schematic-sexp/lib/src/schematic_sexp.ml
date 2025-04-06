open Base
open Schematic
module Sexp = Sexplib.Sexp
include Sexp
include Schematic_sexp_intf

type _ Coding.coding += Coding : t Coding.coding
type Coding.any += Sexp of t

module Coding : Coding.IMPL with type t = t = struct
  let coding = Coding

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

  (* Shamelessly stolen from json-derivers, without the dependency on an
     obsolete version of Base *)
  let rec of_json = function
    | `Null -> Sexplib.Sexp.Atom "Null"
    | `Bool v0 ->
      Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Bool"; sexp_of_bool v0 ]
    | `Float v0 ->
      Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Float"; sexp_of_float v0 ]
    | `String v0 ->
      Sexplib.Sexp.List [ Sexplib.Sexp.Atom "String"; sexp_of_string v0 ]
    | `A v0 ->
      Sexplib.Sexp.List [ Sexplib.Sexp.Atom "A"; sexp_of_list of_json v0 ]
    | `O v0 ->
      Sexplib.Sexp.List
        [
          Sexplib.Sexp.Atom "O";
          sexp_of_list
            (function
              | v0, v1 ->
                let v0 = sexp_of_string v0
                and v1 = of_json v1 in
                Sexplib.Sexp.List [ v0; v1 ])
            v0;
        ]

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

  (* Shamelessly stolen from json-derivers, without the dependency on an
     obsolete version of Base *)
  let rec to_json : t -> Ezjsonm.value option = function
    | Sexplib.Sexp.Atom atom as _sexp -> (
      match atom with
      | "Null" -> Some `Null
      | "Bool" | "Float" | "String" | "A" | "O" | _ -> None)
    | Sexplib.Sexp.List (Sexplib.Sexp.Atom atom :: sexp_args) as _sexp -> (
      try
        match atom with
        | "Bool" as _tag -> (
          match sexp_args with
          | v0 :: [] ->
            let v0 = bool_of_sexp v0 in
            Some (`Bool v0)
          | _ -> None)
        | "Float" as _tag -> (
          match sexp_args with
          | v0 :: [] ->
            let v0 = float_of_sexp v0 in
            Some (`Float v0)
          | _ -> None)
        | "String" as _tag -> (
          match sexp_args with
          | v0 :: [] ->
            let v0 = string_of_sexp v0 in
            Some (`String v0)
          | _ -> None)
        | "A" as _tag -> (
          match sexp_args with
          | v0 :: [] ->
            let v0 = list_of_sexp to_json v0 |> Option.all in
            Option.map ~f:(fun v -> `A v) v0
          | _ -> None)
        | "O" as _tag -> (
          match sexp_args with
          | v0 :: [] ->
            let v0 =
              list_of_sexp
                (function
                  | Sexplib.Sexp.List [ v0; v1 ] ->
                    let v0 = string_of_sexp v0
                    and v1 = to_json v1 in
                    Option.map ~f:(fun v1 -> (v0, v1)) v1
                  | _ -> None)
                v0
              |> Option.all
            in
            Option.map ~f:(fun f -> `O f) v0
          | _ -> None)
        | "Null" -> None
        | _ -> None
      with _ -> None)
    | Sexplib.Sexp.List (Sexplib.Sexp.List _ :: _) -> None
    | Sexplib.Sexp.List [] -> None

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

  module Input = struct
    open Schematic.Streaming.Reader

    let fail fmt =
      let f error = fail (`Content error) in
      Fmt.kstr f fmt

    type token = Sexplib.Parser.token

    let pp_token f = function
      | Sexplib.Parser.STRING s -> Fmt.Dump.string f s
      | LPAREN -> Fmt.string f "("
      | RPAREN -> Fmt.string f ")"
      | EOF -> Fmt.string f "EOF"
      | HASH_SEMI -> Fmt.string f "#;"

    type t = {
      buffer : Buffer.t;
      input : Lexing.lexbuf;
      token : token option;
    }

    let of_lexbuf input = { buffer = Buffer.create 4096; input; token = None }
    let of_channel channel = of_lexbuf @@ Lexing.from_channel channel
    let of_string input = of_lexbuf @@ Lexing.from_string input

    let token = function
      | { token = Some token; _ } as t ->
        ({ t with token = None }, Result.Ok token)
      | { buffer; input; _ } as t ->
        (t, Result.Ok (Sexplib.Lexer.main ~buf:buffer input))

    let push_back token reader =
      let () = assert (Option.is_none reader.token) in
      ({ reader with token = Some token }, Result.Ok false)

    let eof =
      token >>= function
      | EOF -> return ()
      | token -> fail {|expected end of input, got %a|} pp_token token

    let par_l =
      token >>= function
      | LPAREN -> return ()
      | token -> fail {|expected "(", got %a|} pp_token token

    let par_r =
      token >>= function
      | RPAREN -> return ()
      | token -> fail {|expected "(", got %a|} pp_token token

    let par_r_maybe =
      token >>= function RPAREN -> return true | token -> push_back token

    let string =
      let* token in
      match token with
      | STRING v -> return v
      | token -> fail {|expected string, got %a|} pp_token token

    let bool =
      string >>= function
      | "true" -> return true
      | "false" -> return false
      | string -> fail {|expected \"true\" or \"false\", got %S|} string

    let date = `Generic

    let decoded_string name decode =
      let* string in
      match decode string with
      | Some value -> return value
      | None -> fail {|expected %s, got %S|} name string

    let float = decoded_string "float" Float.of_string_opt
    let int = decoded_string "integer" Int.of_string_opt
    let int64 = decoded_string "integer" Int64.of_string_opt
    let json = `Generic

    let list element =
      let* () = par_l in
      let rec elements element =
        par_r_maybe >>= function
        | true -> return []
        | false ->
          let* head = element in
          let+ tail = elements element in
          head :: tail
      in
      elements element

    let object_push = par_l
    let object_pop = par_r

    let object_field_push _ =
      par_r_maybe >>= function
      | true -> return None
      | false ->
        let* () = par_l in
        let* string in
        return (Some string)

    let object_field_pop = par_r

    let option content =
      let* () = par_l in
      par_r_maybe >>= function
      | true -> return None
      | false ->
        let+ content in
        Some content

    let time = `Generic
    let time_local = `Generic
    let tuple_push = par_l
    let tuple_pop = par_r
    let tuple_item_push : (unit, t, _) Schematic.Streaming.Reader.t = return ()
    let tuple_item_pop : (unit, t, _) Schematic.Streaming.Reader.t = return ()

    let union_push cases =
      let* () = par_l in
      let* string in
      match
        List.find cases ~f:(fun (Schematic.Schema.Case { name; _ }) ->
            String.(name = string))
      with
      | None -> fail "no case matches %S" string
      | Some case -> return case

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

    let string_unsafe string t =
      if t.space_needed then t.put " ";
      t.put string;
      { t with space_needed = true }

    let string string =
      let open Sexplib0.Sexp.Private in
      string_unsafe (mach_maybe_esc_str string)

    let par_l t =
      if t.space_needed then t.put " ";
      t.put "(";
      { t with space_needed = false }

    let par_r t =
      t.put ")";
      { t with space_needed = false }

    let encoded_string encode v = string (encode v)
    let bool v = string_unsafe (if v then "true" else "false")
    let date = `Generic
    let eof = return
    let float = encoded_string Float.to_string
    let int = encoded_string Int.to_string
    let int64 = encoded_string Int64.to_string
    let json = `Generic
    let list_push = par_l
    let list_pop = par_r
    let list_item_push = return
    let list_item_pop = return
    let object_push = par_l
    let object_pop = par_r

    let object_field_push name =
      let* () = par_l in
      string name

    let object_field_pop = par_r
    let option_none = string_unsafe "()"
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

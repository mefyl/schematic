(* Highly optimized RFC 8259 compliant JSON scanner. Initially derived from the
   now deprecated jsonm library by Daniel Bünzli <daniel.buenzli@erratique.ch>,
   distributed under the ISC, although significantly rewritten since then.

   Thus:

   Copyright (c) 2012 The jsonm programmers. All rights reserved. Distributed
   under the ISC license, see terms at the end of the file. *)

include Json_scanner_intf

let unsafe_byte s j = String.unsafe_get s j

module Error = struct
  type t =
    | Expected of expected
    | Illegal_escape of illegal_escape
    | Illegal_string_uchar of char
    | Illegal_literal of string
    | Illegal_number of string
    | Unclosed of unclosed

  and expected =
    | Value
    | Name_separator
    | Eoi
    | Array_separator
    | Object_separator

  and illegal_escape =
    | Not_hex of char
    | Not_esc of char
    | Not_lo_surrogate of int
    | Lone_lo_surrogate of int
    | Lone_hi_surrogate of int

  and unclosed =
    | Array
    | Object
    | String

  exception Parse_error of t

  let raise error = raise (Parse_error error)

  let rec pp f = function
    | Illegal_escape illegal_escape ->
      Fmt.pf f "illegal escape: %a" pp_illegal_escape illegal_escape
    | Illegal_string_uchar c ->
      Format.fprintf f "illegal character in JSON string: %c" c
    | Illegal_number n -> Format.fprintf f "illegal number (%s)" n
    | Illegal_literal l -> Format.fprintf f "illegal literal (%s)" l
    | Unclosed unclosed -> Fmt.pf f "unclosed %a" pp_unclosed unclosed
    | Expected expected -> Fmt.pf f "expected %a" pp_expected expected

  and pp_expected f = function
    | Value -> Fmt.string f "JSON value"
    | Name_separator -> Fmt.string f "':'"
    | Array_separator -> Fmt.string f "',' or ']'"
    | Object_separator -> Fmt.string f "object member or '}'"
    | Eoi -> Fmt.string f "end of input"

  and pp_illegal_escape f =
    let pp_cp f u = Format.fprintf f "U+%04X" u in
    function
    | Not_hex c -> Fmt.pf f "%c not a hex digit" c
    | Not_esc c -> Fmt.pf f "%c not an escaped character" c
    | Lone_lo_surrogate p -> Fmt.pf f "%a lone low surrogate" pp_cp p
    | Lone_hi_surrogate p -> Fmt.pf f "%a lone high surrogate" pp_cp p
    | Not_lo_surrogate p -> Fmt.pf f "%a not a low surrogate" pp_cp p

  and pp_unclosed f = function
    | Array -> Fmt.string f "array"
    | Object -> Fmt.string f "object"
    | String -> Fmt.string f "string"
end

type t = {
  mutable at_object_name : bool;
  mutable at_array_start : bool;
  mutable buffer : Buffer.t option;
  mutable root : bool;
  mutable pos : int;
  length : int;
  mutable stack : stack list;
  src : string;
}

and stack =
  | Array
  | Object

(** Whether we're at end of input. *)
let eoi { length; pos; _ } = pos >= length

(** [peekc d] is the current character. Undefined if [eio d]. *)
let peekc { src; pos; _ } = unsafe_byte src pos

(** Skip whitespaces. *)
let skip_ws scanner =
  let is_white = function ' ' | '\t' | '\n' -> true | _ -> false in
  while scanner.pos < scanner.length && is_white (peekc scanner) do
    scanner.pos <- scanner.pos + 1
  done

(** Read a ["\uXXXX"] escape. *)
let rec read_u_escape buffer scanner hi u count =
  let baddc cp =
    if cp <= 0x7F then Buffer.add_char buffer (Char.chr cp)
    else if cp <= 0x7FF then (
      Buffer.add_char buffer (Char.chr (0xC0 lor (cp lsr 6)));
      Buffer.add_char buffer (Char.chr (0x80 lor (cp land 0x3F))))
    else if cp <= 0xFFFF then (
      Buffer.add_char buffer (Char.chr (0xE0 lor (cp lsr 12)));
      Buffer.add_char buffer (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
      Buffer.add_char buffer (Char.chr (0x80 lor (cp land 0x3F))))
    else (
      Buffer.add_char buffer (Char.chr (0xF0 lor (cp lsr 18)));
      Buffer.add_char buffer (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
      Buffer.add_char buffer (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
      Buffer.add_char buffer (Char.chr (0x80 lor (cp land 0x3F))))
  and err_lone_hi p = Error.raise (Illegal_escape (Lone_hi_surrogate p)) in
  if count > 0 then
    if eoi scanner then Error.raise (Unclosed String)
    else
      match peekc scanner with
      | ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F') as c ->
        let u =
          (u * 16)
          +
          if c <= '9' then Char.code c - 0x30
          else if c <= 'F' then Char.code c - 0x37
          else Char.code c - 0x57
        in
        scanner.pos <- scanner.pos + 1;
        read_u_escape buffer scanner hi u (count - 1)
      | c -> Error.raise (Illegal_escape (Not_hex c))
  else
    match hi with
    | Some hi ->
      if u < 0xDC00 || u > 0xDFFF then
        Error.raise (Illegal_escape (Not_lo_surrogate u))
      else
        let u = (((hi land 0x3FF) lsl 10) lor (u land 0x3FF)) + 0x10000 in
        baddc u
    | None ->
      if u < 0xD800 || u > 0xDFFF then baddc u
      else if u > 0xDBFF then Error.raise (Illegal_escape (Lone_lo_surrogate u))
      else if eoi scanner || peekc scanner <> '\\' then err_lone_hi u
      else begin
        scanner.pos <- scanner.pos + 1;
        if eoi scanner || peekc scanner <> 'u' then err_lone_hi u
        else begin
          scanner.pos <- scanner.pos + 1;
          read_u_escape buffer scanner (Some u) 0 4
        end
      end

(** Read a ["\..."] escape. *)
and read_escape buffer scanner =
  if eoi scanner then Error.raise (Unclosed String);
  let c = peekc scanner in
  scanner.pos <- scanner.pos + 1;
  match c with
  | '"' -> Buffer.add_char buffer '"'
  | '\\' -> Buffer.add_char buffer '\\'
  | '/' -> Buffer.add_char buffer '/'
  | 'b' -> Buffer.add_char buffer '\x08'
  | 'f' -> Buffer.add_char buffer '\x0C'
  | 'n' -> Buffer.add_char buffer '\n'
  | 'r' -> Buffer.add_char buffer '\r'
  | 't' -> Buffer.add_char buffer '\t'
  | 'u' -> read_u_escape buffer scanner None 0 4
  | c -> Error.raise (Illegal_escape (Not_esc c))

let get_buffer scanner = function
  | Some buffer -> buffer
  | None -> (
    match scanner.buffer with
    | Some buffer ->
      let () = Buffer.clear buffer in
      buffer
    | None ->
      let buffer = Buffer.create 4096 in
      let () = scanner.buffer <- Some buffer in
      buffer)

(** Read a string literal. If it is pure UTF-8 with no escapes, extract it
    directly with no intermediate copies. *)
let read_string scanner =
  let err_str_char c = Error.raise (Illegal_string_uchar c) in
  let rec read_chunk buffer =
    let s = scanner.src
    and start = scanner.pos
    and n = scanner.length in
    let i =
      let rec skip i =
        if i >= n then i
        else
          let c = unsafe_byte s i in
          if c >= ' ' && c <> '"' && c <> '\\' && c < '\x80' then skip (i + 1)
          else
            match c with
            | '\x80' .. '\xBF' | '\xF8' .. '\xFF' ->
              err_str_char c (* lone continuation or invalid leading byte *)
            | '\xC0' .. '\xDF' as b0 ->
              (* 2-byte sequence *)
              if i + 1 >= n then Error.raise (Unclosed String);
              let b1 = unsafe_byte s (i + 1) in
              if Char.code b1 land 0xC0 <> 0x80 then err_str_char b1;
              let cp =
                ((Char.code b0 land 0x1F) lsl 6) lor (Char.code b1 land 0x3F)
              in
              if cp < 0x80 then err_str_char b0 else skip (i + 2)
            | '\xE0' .. '\xEF' as b0 ->
              (* 3-byte sequence *)
              if i + 2 >= n then Error.raise (Unclosed String);
              let b1 = unsafe_byte s (i + 1)
              and b2 = unsafe_byte s (i + 2) in
              if Char.code b1 land 0xC0 <> 0x80 then err_str_char b1
              else if Char.code b2 land 0xC0 <> 0x80 then err_str_char b2
              else
                let cp =
                  ((Char.code b0 land 0x0F) lsl 12)
                  lor ((Char.code b1 land 0x3F) lsl 6)
                  lor (Char.code b2 land 0x3F)
                in
                if cp < 0x800 then err_str_char b0
                else if 0xD800 <= cp && cp <= 0xDFFF then err_str_char b0
                else skip (i + 3)
            | '\xF0' .. '\xF7' as b0 ->
              (* 4-byte sequence *)
              if i + 3 >= n then Error.raise (Unclosed String)
              else
                let b1 = unsafe_byte s (i + 1)
                and b2 = unsafe_byte s (i + 2)
                and b3 = unsafe_byte s (i + 3) in
                if Char.code b1 land 0xC0 <> 0x80 then err_str_char b1
                else if Char.code b2 land 0xC0 <> 0x80 then err_str_char b2
                else if Char.code b3 land 0xC0 <> 0x80 then err_str_char b3
                else
                  let cp =
                    ((Char.code b0 land 0x07) lsl 18)
                    lor ((Char.code b1 land 0x3F) lsl 12)
                    lor ((Char.code b2 land 0x3F) lsl 6)
                    lor (Char.code b3 land 0x3F)
                  in
                  if cp < 0x10000 || cp > 0x10FFFF then err_str_char b0
                  else skip (i + 4)
            | _ -> i
      in
      skip start
    in
    if i >= n then Error.raise (Unclosed String)
    else
      let c = unsafe_byte s i in
      if c = '"' then
        let () = scanner.pos <- i + 1 in
        match buffer with
        | None -> String.sub s start (i - start)
        | Some buffer ->
          let () = Buffer.add_substring buffer s start (i - start) in
          Buffer.contents buffer
      else
        let buffer = get_buffer scanner buffer in
        let () = Buffer.add_substring buffer s start (i - start) in
        if c = '\\' then
          let () = scanner.pos <- i + 1 in
          let () = read_escape buffer scanner in
          read_chunk (Some buffer)
        else err_str_char c
  in
  read_chunk None

let is_val_sep = function
  | ' ' | '\t' | '\n' | ',' | ']' | '}' -> true
  | _ -> false

(** Read a literal float. *)
let read_float scanner =
  let start = scanner.pos in
  let rec loop i =
    if i < scanner.length && not (is_val_sep (unsafe_byte scanner.src i)) then
      loop (i + 1)
    else
      let repr = String.sub scanner.src start (i - start)
      and () = scanner.pos <- i in
      try Float (float_of_string repr)
      with Failure _ -> Error.raise (Illegal_number repr)
  in
  loop start

(** Read null, true or false literals. *)
let read_literal scanner =
  let start = scanner.pos in
  let rec loop i =
    if i < scanner.length && not (is_val_sep (unsafe_byte scanner.src i)) then
      loop (i + 1)
    else
      let repr = String.sub scanner.src start (i - start)
      and () = scanner.pos <- i in
      match repr with
      | "true" -> Bool true
      | "false" -> Bool false
      | "null" -> Null
      | s -> Error.raise (Illegal_literal s)
  in
  loop start

(** Read a JSON value. *)
let read_value scanner =
  if eoi scanner then Error.raise (Expected Value)
  else
    match peekc scanner with
    | '[' ->
      scanner.at_array_start <- true;
      scanner.stack <- Array :: scanner.stack;
      scanner.pos <- scanner.pos + 1;
      Array_start
    | '{' ->
      scanner.at_array_start <- true;
      scanner.at_object_name <- true;
      scanner.stack <- Object :: scanner.stack;
      scanner.pos <- scanner.pos + 1;
      Object_start
    | '"' ->
      scanner.pos <- scanner.pos + 1;
      String (read_string scanner)
    | 'f' | 'n' | 't' -> read_literal scanner
    | '0' .. '9' | '-' -> read_float scanner
    | _ -> Error.raise (Expected Value)

let pop scanner =
  match scanner.stack with
  | _ :: (Object :: _ as ss) ->
    let () = scanner.at_object_name <- true in
    scanner.stack <- ss
  | _ :: ss ->
    let () = scanner.at_object_name <- false in
    scanner.stack <- ss
  | [] -> assert false

let read_array_item scanner =
  let at_array_start = scanner.at_array_start in
  scanner.at_array_start <- false;
  if eoi scanner then Error.raise (Unclosed Array);
  match peekc scanner with
  | ']' ->
    let () = pop scanner
    and () = scanner.pos <- scanner.pos + 1 in
    Array_end
  | ',' when not at_array_start ->
    scanner.pos <- scanner.pos + 1;
    let () = skip_ws scanner in
    read_value scanner
  | _ when at_array_start -> read_value scanner
  | _ -> Error.raise (Expected Array_separator)

let read_object_value scanner =
  let () = scanner.at_object_name <- true in
  if (not (eoi scanner)) && peekc scanner = ':' then
    let () = scanner.pos <- scanner.pos + 1 in
    let () = skip_ws scanner in
    read_value scanner
  else Error.raise (Expected Name_separator)

let read_object_name scanner =
  let r_name scanner =
    if eoi scanner || peekc scanner <> '"' then
      Error.raise (Expected Object_separator)
    else (
      scanner.pos <- scanner.pos + 1;
      Name (read_string scanner))
  in
  let at_array_start = scanner.at_array_start in
  scanner.at_array_start <- false;
  scanner.at_object_name <- false;
  if eoi scanner then Error.raise (Unclosed Object);
  let c = peekc scanner in
  if c = '}' then
    let () = pop scanner
    and () = scanner.pos <- scanner.pos + 1 in
    Object_end
  else if at_array_start then r_name scanner
  else if c = ',' then
    let () = scanner.pos <- scanner.pos + 1
    and () = skip_ws scanner in
    r_name scanner
  else Error.raise (Expected Object_separator)

let of_string src =
  {
    at_object_name = false;
    at_array_start = false;
    buffer = None;
    root = true;
    src;
    pos = 0;
    length = String.length src;
    stack = [];
  }

let scan scanner =
  let () = skip_ws scanner in
  match scanner.stack with
  | Array :: _ -> read_array_item scanner
  | Object :: _ when scanner.at_object_name -> read_object_name scanner
  | Object :: _ -> read_object_value scanner
  | [] when scanner.root ->
    let () = scanner.root <- false in
    read_value scanner
  | [] -> if eoi scanner then End else Error.raise (Expected Eoi)

(*---------------------------------------------------------------------------
  Copyright (c) 2012 The jsonm programmers

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
  REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
  AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
  INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
  OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
  PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

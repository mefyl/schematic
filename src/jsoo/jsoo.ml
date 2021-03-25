module Impl_js = Json.Impl_js
open Base
open Js_of_ocaml
open Poly
open Schematic.Schema
include Schematic.Schema.Schemas

let ( == ) = Caml.( == )

type decode_error = [ `Js_decode of string ]

type bool_js = bool Js.t

type int_js = Js.number Js.t

type any_js = Js.Unsafe.any

type unit_js = Js.Unsafe.any Js.Opt.t

type ignore_js = Js.Unsafe.any

module Either = struct
  include Either

  type (_, _) js = Js.Unsafe.any
end

module Ptime = struct
  include Ptime

  type js = Js.date Js.t

  type date_js = Js.date Js.t

  type span_js = int Js.t
end

module Uri = struct
  include Uri

  type js = Js.js_string Js.t
end

type 'a list_js = 'a Js.js_array Js.t

type 'a option_js = 'a Js.opt

type string_js = Js.js_string Js.t

let date_of_js (date : Js.date Js.t) =
  (date##getFullYear, date##getMonth + 1, date##getDate)

let ptime_of_js (date : Js.date Js.t) =
  match Ptime.of_float_s (date##getTime /. 1000.) with
  | Some time -> time
  | None -> failwith ("invalid time: " ^ Float.to_string date##getTime)

let decode schema =
  let rec decode :
      type t kind.
      string option ->
      string list ->
      (t, kind) descriptor ->
      Js.Unsafe.any ->
      (t, 'error) Result.t =
    let open Result in
    fun id ids descriptor js ->
      let fail fmt =
        let schema = id in
        let f s =
          Result.fail
            (`Js_decode
              Fmt.(
                str "schema %a%a: %s"
                  (list ~sep:(const char '.') string)
                  (List.rev ids)
                  (option (const string " (" ++ string ++ const string ")"))
                  schema s))
        in
        Fmt.kstr f fmt
      in
      match descriptor with
      | Any -> Result.return @@ Impl_js.of_js js
      | Bool -> Result.return @@ Js.to_bool (Js.Unsafe.coerce js)
      | Date ->
        if Js.instanceof js Js.Unsafe.global##._Date then
          Result.return @@ date_of_js @@ Js.Unsafe.coerce js
        else
          fail "invalid date: %s" (Js._JSON##stringify js |> Js.to_string)
      | Float ->
        if Js.typeof js == Js.string "number" then
          Result.return (Js.Unsafe.coerce js |> Js.float_of_number)
        else
          fail "invalid number"
      | Ignore -> Result.return ()
      | Int ->
        if Js.typeof js == Js.string "number" then
          Result.return
            (Js.Unsafe.coerce js |> Js.float_of_number |> Int.of_float)
        else
          fail "invalid integer"
      | Int64 ->
        if Js.typeof js == Js.string "number" then
          Result.return
            (Js.Unsafe.coerce js |> Js.float_of_number |> Int64.of_float)
        else
          fail "invalid integer"
      | List items_schema ->
        Js.Unsafe.coerce js |> Js.to_array
        |> Array.map ~f:(decode items_schema.id ids items_schema.descriptor)
        |> Array.to_list |> Result.all
      | Map (cons, _, schema) ->
        decode id ids schema js >>= fun value -> Result.return @@ cons value
      | Mapping _ -> failwith "FIXME"
      | Null ->
        if js == Js.Unsafe.pure_js_expr "null" then
          Result.return ()
        else
          let constr = Js.Unsafe.global##._String in
          fail "expected null, got %s" (new%js constr js |> Js.to_string)
      | Nullable s ->
        Js.Opt.case (Js.Opt.return js)
          (fun () -> Result.return None)
          (fun v ->
            decode s.id ids s.descriptor v |> Result.map ~f:Option.return)
      | Object descriptor ->
        let rec decode_object :
            type t.
            (t, [ `Object ]) descriptor -> (t, [> decode_error ]) Result.t =
          function
          | Null -> Result.return ()
          | Object { field; requirement; omit; schema; rest; _ } ->
            let inline, schema = extract_field_schema schema in
            let field =
              let missing =
                if inline then
                  let expected =
                    Schematic.Schema.descriptor_fields schema.descriptor
                  in
                  Option.is_some
                  @@ List.find
                       ~f:(fun f ->
                         not
                           (Js.Unsafe.get js (Js.string f)
                           == Js.Unsafe.pure_js_expr "undefined"))
                       expected
                else
                  false
              in
              if missing then
                decode_field omit id ids field schema requirement None
              else
                let value =
                  if inline then
                    Some js
                  else
                    Js.Unsafe.get js (Js.string field) |> Js.Optdef.to_option
                in
                decode_field omit id ids field schema requirement value
            in
            Result.Let_syntax.Let_syntax.both field (decode_object rest)
        in
        decode_object (Object descriptor)
      | String -> Result.return @@ Js.to_string @@ Js.Unsafe.coerce js
      | String_const s ->
        if Js.Unsafe.coerce js == Js.string s then
          Result.return ()
        else
          Result.fail @@ `Js_decode "mismatched constant string"
      | Time -> Result.return @@ ptime_of_js @@ Js.Unsafe.coerce js
      | Time_local -> Result.return @@ (ptime_of_js @@ Js.Unsafe.coerce js, 0)
      | Tuple descriptor ->
        let rec decode_tuple :
            type t. int -> (t, [ `Tuple ]) descriptor -> (t, 'error) Result.t =
         fun i -> function
          | Tuple { schema; rest } ->
            decode schema.id (Int.to_string i :: ids) schema.descriptor
            @@ Js.Unsafe.get (Js.Unsafe.coerce js) i
            >>= fun hd ->
            decode_tuple (i + 1) rest >>= fun tl -> Result.return (hd, tl)
          | Null -> Result.return ()
        in
        decode_tuple 0 (Tuple descriptor)
      | Union { cases; key } -> (
        let conv (Case { decoder; name; schema; _ }) =
          let key =
            match key with
            | None -> true
            | Some key ->
              phys_equal (Js.Unsafe.get js (Js.string key)) (Js.string name)
          in
          if key then
            match decode schema.id ids schema.descriptor js with
            | Result.Ok v -> Some (decoder v)
            | Result.Error _ -> None
          else
            None
        in
        match List.find_map ~f:conv cases with
        | Some res -> Result.return res
        | None -> fail "no union case was matched")
  and decode_field :
      type t r.
      bool ->
      string option ->
      string list ->
      string ->
      t schema ->
      (t, r) field_requirement ->
      Js.Unsafe.any option ->
      (r, 'error) Result.t =
   fun omit id ids field schema ->
    let ids = field :: ids in
    function
    | Required -> (
      function
      | Some v -> decode schema.id ids schema.descriptor v
      | None -> (
        match id with
        | None ->
          Result.fail
            (`Js_decode (Printf.sprintf "missing required field %S" field))
        | Some id ->
          Result.fail
            (`Js_decode
              (Printf.sprintf "missing required field %S in schema %S" field id))
        ))
    | Default default -> (
      function
      | Some v ->
        if omit then
          Result.return default
        else
          decode schema.id ids schema.descriptor v
      | None -> Result.return default)
    | Optional -> (
      function
      | Some v ->
        if v == Js.Unsafe.pure_js_expr "null" then
          match schema.descriptor with
          | Null -> Result.return (Some ())
          | Nullable _ -> Result.return (Some None)
          | _ -> Result.return None
        else
          Result.map ~f:Option.return
          @@ decode schema.id ids schema.descriptor v
      | None -> Result.return None)
  in
  let ids =
    match schema.id with
    | Some id -> [ id ]
    | None -> []
  in
  decode schema.id ids schema.descriptor

let js_of_date (y, m, d) = new%js Js.date_day y (m - 1) d

let js_of_ptime t = new%js Js.date_fromTimeValue (Ptime.to_float_s t *. 1000.)

let encode schema =
  let rec encode : type t kind. (t, kind) descriptor -> t -> Js.Unsafe.any =
   fun schema v ->
    match schema with
    | Any -> Js.Unsafe.coerce @@ Impl_js.to_js v
    | Bool -> Js.Unsafe.coerce @@ Js.bool v
    | Date -> Js.Unsafe.coerce @@ js_of_date v
    | Float -> Js.Unsafe.coerce @@ Js.number_of_float v
    | Ignore -> Js.Unsafe.coerce @@ Js.Unsafe.pure_js_expr "null"
    | Int -> Js.Unsafe.coerce @@ Js.number_of_float @@ Float.of_int v
    | Int64 -> Js.Unsafe.coerce @@ Js.number_of_float @@ Float.of_int64 v
    | List items_schema ->
      Js.Unsafe.coerce @@ Js.array @@ Array.of_list
      @@ List.map ~f:(encode items_schema.descriptor) v
    | Map (_, decons, schema) -> encode schema @@ decons v
    | Null -> Js.Unsafe.coerce @@ Js.Unsafe.pure_js_expr "null"
    | Nullable s -> (
      match v with
      | None -> Js.Unsafe.pure_js_expr "null"
      | Some v -> encode s.descriptor v)
    | Mapping _ -> failwith "FIXME"
    | Object descriptor ->
      let rec encode_object :
          type t. Js.Unsafe.any -> (t, [ `Object ]) descriptor -> t -> unit =
        let encode_field : type t r. (t, r) field_requirement -> r -> t option =
          function
          | Required -> Option.some
          | Default default -> (
            function
            | v when v = default -> None
            | v -> Some v)
          | Optional -> Fn.id
        in
        fun into schema v ->
          match schema with
          | Null -> ()
          | Object
              {
                schema =
                  ( Outline { descriptor = Ignore; _ }
                  | Inline { descriptor = Ignore; _ } );
                rest;
                _;
              } ->
            encode_object into rest (snd v)
          | Object { field; requirement; schema; rest; _ } ->
            let inline, schema = extract_field_schema schema in
            let () =
              match encode_field requirement (fst v) with
              | None -> ()
              | Some v ->
                if inline then
                  match schema.descriptor with
                  | Null -> ()
                  | Object descriptor ->
                    encode_object into (Object descriptor) v
                  | Map (_, f, Object d) -> encode_object into (Object d) (f v)
                  | _ -> failwith "invalid object schema"
                else
                  Js.Unsafe.set into (Js.string field)
                    (encode schema.descriptor v)
            in
            encode_object into rest (snd v)
      in
      let o = Js.Unsafe.coerce (object%js end) in
      let () = encode_object o (Object descriptor) v in
      o
    | Tuple descriptor ->
      let res = new%js Js.array_empty in
      let rec encode_tuple : type t. t -> (t, [ `Tuple ]) descriptor -> unit =
       fun v -> function
        | Null -> ()
        | Tuple { schema; rest } ->
          let _ = res##push (encode schema.descriptor (fst v)) in
          encode_tuple (snd v) rest
      in
      let () = encode_tuple v (Tuple descriptor) in
      Js.Unsafe.coerce res
    | String -> Js.Unsafe.coerce @@ Js.string v
    | String_const v -> Js.Unsafe.coerce @@ Js.string v
    | Time -> Js.Unsafe.coerce @@ js_of_ptime v
    | Time_local -> Js.Unsafe.coerce @@ js_of_ptime (fst v)
    | Union { cases; key } -> (
      let conv (Case { encoder; name; schema; _ }) =
        match encoder v with
        | Some v ->
          let res = encode schema.descriptor v in
          let () =
            match key with
            | None -> ()
            | Some key -> Js.Unsafe.set res (Js.string key) (Js.string name)
          in
          Some res
        | None -> None
      in
      match List.find_map ~f:conv cases with
      | Some res -> res
      | None -> failwith "unmatched variant schema case")
  in
  encode schema.descriptor

module Impl_js = Json.Impl_js
open Base
open Js_of_ocaml
open Poly

let ( == ) = Caml.( == )

type bool_js = bool Js.t

type int_js = Js.number Js.t

module Ptime = struct
  include Schematic.Schema.Ptime

  type js = Js.date Js.t

  type date_js = Js.date Js.t
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
      type t.
      t Schematic.Schema.descriptor -> Js.Unsafe.any -> (t, 'error) Result.t =
   fun schema js ->
    let open Result in
    match schema with
    | Schematic.Schema.Any -> Result.return @@ Impl_js.of_js js
    | Schematic.Schema.Bool -> Result.return @@ Js.to_bool (Js.Unsafe.coerce js)
    | Schematic.Schema.Date ->
      Result.return @@ date_of_js @@ Js.Unsafe.coerce js
    | Schematic.Schema.End -> Result.return ()
    | Schematic.Schema.Int ->
      Result.return @@ Int.of_float @@ Js.float_of_number @@ Js.Unsafe.coerce js
    | Schematic.Schema.List items_schema ->
      Js.Unsafe.coerce js |> Js.to_array
      |> Array.map ~f:(decode items_schema.descriptor)
      |> Array.to_list |> Result.all
    | Schematic.Schema.Map (cons, _, schema) ->
      decode schema js >>= fun value -> Result.return @@ cons value
    | Schematic.Schema.Mapping _ -> failwith "FIXME"
    | Schematic.Schema.Null -> Result.return ()
    | Schematic.Schema.Object { field; requirement; schema; rest; inline; _ } ->
      let value =
        if inline then
          Some js
        else
          Js.Unsafe.get js (Js.string field) |> Js.Opt.to_option
      in
      let field = decode_field field schema requirement value in
      Result.Let_syntax.Let_syntax.both field (decode rest js)
    | Schematic.Schema.String ->
      Result.return @@ Js.to_string @@ Js.Unsafe.coerce js
    | Schematic.Schema.String_const s ->
      if Js.Unsafe.coerce js == Js.string s then
        Result.return ()
      else
        Result.fail @@ `Js_decode "mismatched constant string"
    | Schematic.Schema.Time ->
      Result.return @@ ptime_of_js @@ Js.Unsafe.coerce js
    | Schematic.Schema.Time_local ->
      Result.return @@ (ptime_of_js @@ Js.Unsafe.coerce js, 0)
    | Schematic.Schema.Tuple _ ->
      let rec decode_tuple :
          type t. int -> t Schematic.Schema.descriptor -> (t, 'error) Result.t =
       fun i -> function
        | Schematic.Schema.Tuple { schema; rest } ->
          decode schema.descriptor @@ Js.Unsafe.get (Js.Unsafe.coerce js) i
          >>= fun hd ->
          decode_tuple (i + 1) rest >>= fun tl -> Result.return (hd, tl)
        | Schematic.Schema.Null -> Result.return ()
        | _ -> failwith "unexpected schema in a tuple"
      in
      decode_tuple 0 schema
    | Schematic.Schema.Union cases -> (
      let conv (Schematic.Schema.Case { schema; decoder; _ }) =
        match decode schema.descriptor js with
        | Result.Ok v -> Some (decoder v)
        | Result.Error _ -> None
      in
      match List.find_map ~f:conv cases with
      | Some res -> Result.return res
      | None -> Result.fail (`Js_decode "no union case was matched") )
  and decode_field :
      type t r.
      string ->
      t Schematic.Schema.schema ->
      (t, r) Schematic.Schema.field_requirement ->
      Js.Unsafe.any option ->
      (r, 'error) Result.t =
   fun field schema -> function
    | Schematic.Schema.Required -> (
      function
      | Some v -> decode schema.descriptor v
      | None ->
        Result.fail
          (`Js_decode (Printf.sprintf "missing required field %s" field)) )
    | Schematic.Schema.Default default -> (
      function
      | Some v -> decode schema.descriptor v
      | None -> Result.return default )
    | Schematic.Schema.Optional -> (
      function
      | Some v ->
        if v == Js.Unsafe.pure_js_expr "null" then
          Result.return None
        else
          Result.map ~f:Option.return @@ decode schema.descriptor v
      | None -> Result.return None )
  in
  decode schema.Schematic.Schema.descriptor

let js_of_date (y, m, d) = new%js Js.date_day y (m - 1) d

let js_of_ptime t = new%js Js.date_fromTimeValue (Ptime.to_float_s t *. 1000.)

let encode schema =
  let rec encode : type t. t Schematic.Schema.descriptor -> t -> Js.Unsafe.any =
   fun schema v ->
    match schema with
    | Schematic.Schema.Any -> Js.Unsafe.coerce @@ Impl_js.to_js v
    | Schematic.Schema.Bool -> Js.Unsafe.coerce @@ Js.bool v
    | Schematic.Schema.Date -> Js.Unsafe.coerce @@ js_of_date v
    | Schematic.Schema.End -> Js.Unsafe.coerce @@ Js.Unsafe.pure_js_expr "null"
    | Schematic.Schema.Int ->
      Js.Unsafe.coerce @@ Js.number_of_float @@ Float.of_int v
    | Schematic.Schema.List items_schema ->
      Js.Unsafe.coerce @@ Js.array @@ Array.of_list
      @@ List.map ~f:(encode items_schema.descriptor) v
    | Schematic.Schema.Map (_, decons, schema) -> encode schema @@ decons v
    | Schematic.Schema.Null -> Js.Unsafe.coerce @@ Js.Unsafe.pure_js_expr "null"
    | Schematic.Schema.Mapping _ -> failwith "FIXME"
    | Schematic.Schema.Object _ ->
      let rec encode_object :
          type t. Js.Unsafe.any -> t Schematic.Schema.descriptor -> t -> unit =
        let encode_field :
            type t r. (t, r) Schematic.Schema.field_requirement -> r -> t option
            = function
          | Schematic.Schema.Required -> Option.some
          | Schematic.Schema.Default default -> (
            function
            | v when v = default -> None
            | v -> Some v )
          | Schematic.Schema.Optional -> Fn.id
        in
        fun into schema v ->
          match schema with
          | Schematic.Schema.End -> ()
          | Schematic.Schema.Object
              { field; requirement; schema; rest; inline; _ } ->
            let () =
              match encode_field requirement (fst v) with
              | None -> ()
              | Some v ->
                if inline then
                  encode_object into schema.descriptor v
                else
                  Js.Unsafe.set into (Js.string field)
                    (encode schema.descriptor v)
            in
            encode_object into rest (snd v)
          | _ -> failwith "invalid object schema"
      in
      let o = Js.Unsafe.coerce (object%js end) in
      let () = encode_object o schema v in
      o
    | Schematic.Schema.Tuple _ ->
      let res = new%js Js.array_empty in
      let rec encode_tuple : type t. t -> t Schematic.Schema.descriptor -> unit
          =
       fun v -> function
        | Schematic.Schema.End -> ()
        | Schematic.Schema.Tuple { schema; rest } ->
          let _ = res##push (encode schema.descriptor (fst v)) in
          encode_tuple (snd v) rest
        | _ -> failwith "invalid tuple schema"
      in
      let () = encode_tuple v schema in
      Js.Unsafe.coerce res
    | Schematic.Schema.String -> Js.Unsafe.coerce @@ Js.string v
    | Schematic.Schema.String_const v -> Js.Unsafe.coerce @@ Js.string v
    | Schematic.Schema.Time -> Js.Unsafe.coerce @@ js_of_ptime v
    | Schematic.Schema.Time_local -> Js.Unsafe.coerce @@ js_of_ptime (fst v)
    | Schematic.Schema.Union cases -> (
      let conv (Schematic.Schema.Case { schema; encoder; _ }) =
        match encoder v with
        | Some v -> Some (encode schema.descriptor v)
        | None -> None
      in
      match List.find_map ~f:conv cases with
      | Some res -> res
      | None -> failwith "unmatched variant schema case" )
  in
  encode schema.Schematic.Schema.descriptor

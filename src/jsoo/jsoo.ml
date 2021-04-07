module Impl_js = Json.Impl_js
open Base
open Js_of_ocaml
open Schematic.Schema
include Schematic.Schema.Schemas

let date_of_js (date : Js.date Js.t) =
  (date##getFullYear, date##getMonth + 1, date##getDate)

let ptime_of_js (date : Js.date Js.t) =
  match Ptime.of_float_s (date##getTime /. 1000.) with
  | Some time -> time
  | None -> failwith ("invalid time: " ^ Float.to_string date##getTime)

let js_of_date (y, m, d) = new%js Js.date_day y (m - 1) d

let js_of_ptime t = new%js Js.date_fromTimeValue (Ptime.to_float_s t *. 1000.)

module Jsoo_coding : Schematic.Coding.S with type t = Js.Unsafe.any = struct
  type t = Js.Unsafe.any

  let type_name o = Js_of_ocaml.Js.typeof o |> Js.to_string

  let set_key =
    let f o f v =
      try
        if Base.phys_equal (Js_of_ocaml.Js.typeof o) (Js.string "object") then
          let () = Js.Unsafe.set o (Js.string f) v in
          Result.return o
        else
          failwith "null or undefined"
      with
      | _ -> Result.fail @@ Schematic.Coding.type_error "object" (type_name o)
    in
    `Custom f

  let null = Js.Unsafe.pure_js_expr "null"

  let is_null v = Base.phys_equal v null

  let of_bool v = Js.Unsafe.coerce @@ Js.bool v

  let of_fields fields =
    let res = Js.Unsafe.coerce (object%js end) in
    let () =
      let f (field, v) = Js.Unsafe.set res (Js.string field) v in
      List.iter ~f fields
    in
    res

  let of_float v = Js.Unsafe.coerce @@ Js.number_of_float v

  let of_int v = Js.Unsafe.coerce @@ Js.number_of_float @@ Float.of_int v

  let of_int64 v = Js.Unsafe.coerce @@ Js.number_of_float @@ Float.of_int64 v

  let of_json v = Js.Unsafe.coerce @@ Impl_js.to_js v

  let of_list v = Js.Unsafe.coerce @@ Js.array @@ Array.of_list v

  let of_string v = Js.Unsafe.coerce @@ Js.string v

  let of_time v = Js.Unsafe.coerce @@ js_of_ptime v

  let of_time_local v = Js.Unsafe.coerce @@ js_of_ptime (fst v)

  let to_bool _ = failwith "not implemented bool"

  let to_fields o =
    let f key = (Js.to_string key, Js_of_ocaml.Js.Unsafe.get o key) in
    Option.return @@ List.map ~f @@ Array.to_list @@ Js.to_array
    @@ Js_of_ocaml.Js.object_keys o

  let to_float _ = failwith "not implemented float"

  let to_int _ = failwith "not implemented int"

  let to_int64 _ = failwith "not implemented int64"

  let to_json _ = failwith "not implemented json"

  let to_list _ = failwith "not implemented list"

  let to_string _ = failwith "not implemented string"

  let to_time _ = failwith "not implemented time"

  let to_time_local _ = failwith "not implemented time_local"
end

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

let decode schema =
  let ( >>= ) = Result.( >>= ) in
  let fail id ids fmt =
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
  let rec decode :
      type t.
      string option ->
      string list ->
      t descriptor ->
      Js.Unsafe.any ->
      (t, 'error) Result.t =
   fun id ids descriptor js ->
    let fail fmt = fail id ids fmt in
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
        Result.return (Js.Unsafe.coerce js |> Js.float_of_number |> Int.of_float)
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
        (fun v -> decode s.id ids s.descriptor v |> Result.map ~f:Option.return)
    | Object fields ->
      let rec decode_object : type t. t field -> (t, [> decode_error ]) Result.t
          = function
        | FieldEnd -> Result.return ()
        | Field { field; requirement; omit; schema; rest; _ } ->
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
                       Js.Unsafe.get js (Js.string f)
                       == Js.Unsafe.pure_js_expr "undefined")
                     expected
              else
                false
            in
            if missing then
              decode_field omit schema.id ids field schema requirement None
            else
              let value =
                if inline then
                  Some js
                else
                  Js.Unsafe.get js (Js.string field) |> Js.Optdef.to_option
              in
              decode_field omit schema.id ids field schema requirement value
          in
          Result.Let_syntax.Let_syntax.both field (decode_object rest)
      in
      decode_object fields
    | String -> Result.return @@ Js.to_string @@ Js.Unsafe.coerce js
    | String_const s ->
      if Js.Unsafe.coerce js == Js.string s then
        Result.return ()
      else
        fail "mismatched constant string"
    | Time -> Result.return @@ ptime_of_js @@ Js.Unsafe.coerce js
    | Time_local -> Result.return @@ (ptime_of_js @@ Js.Unsafe.coerce js, 0)
    | Tuple items ->
      let rec decode_tuple : type t. int -> t tuple -> (t, 'error) Result.t =
       fun i -> function
        | Item { schema; rest } ->
          decode schema.id (Int.to_string i :: ids) schema.descriptor
          @@ Js.Unsafe.get (Js.Unsafe.coerce js) i
          >>= fun hd ->
          decode_tuple (i + 1) rest >>= fun tl -> Result.return (hd, tl)
        | ItemEnd -> Result.return ()
      in
      decode_tuple 0 items
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
    let fail fmt = fail id ids fmt in
    function
    | Required -> (
      function
      | Some v -> decode schema.id ids schema.descriptor v
      | None -> (
        match id with
        | None -> fail "missing required field %S" field
        | Some id -> fail "missing required field %S in schema %S" field id))
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

module Coding = Schematic.Coding.Make (Jsoo_coding)

let encode = Coding.encode

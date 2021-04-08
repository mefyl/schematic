module Impl_js = Json.Impl_js
open Base
open Js_of_ocaml
include Schematic.Schema.Schemas

let date_of_js (date : Js.date Js.t) =
  (date##getFullYear, date##getMonth + 1, date##getDate)

let ptime_of_js (date : Js.date Js.t) =
  match Ptime.of_float_s (date##getTime /. 1000.) with
  | Some time -> time
  | None -> failwith ("invalid time: " ^ Float.to_string date##getTime)

let js_of_date (y, m, d) = new%js Js.date_day y (m - 1) d

let js_of_ptime t = new%js Js.date_fromTimeValue (Ptime.to_float_s t *. 1000.)

let ( == ) = Base.phys_equal

let ( let+ ) = Option.( >>| )

module Jsoo_coding : Schematic.Coding.S with type t = Js.Unsafe.any = struct
  type t = Js.Unsafe.any

  let type_name o = Js_of_ocaml.Js.typeof o |> Js.to_string

  let set_key =
    let f o f v =
      try
        if Js_of_ocaml.Js.typeof o == Js.string "object" then
          let () = Js.Unsafe.set o (Js.string f) v in
          Result.return o
        else
          failwith "null or undefined"
      with
      | _ -> Result.fail @@ Schematic.Coding.type_error "object" (type_name o)
    in
    `Custom f

  let null = Js.Unsafe.pure_js_expr "null"

  let is_null v = v == null

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

  let to_bool v =
    if Js.typeof v == Js.string "boolean" then
      Js.Unsafe.coerce v |> Js.to_bool |> Option.return
    else
      None

  let to_fields o =
    let f key = (Js.to_string key, Js_of_ocaml.Js.Unsafe.get o key) in
    Option.return @@ List.map ~f @@ Array.to_list @@ Js.to_array
    @@ Js_of_ocaml.Js.object_keys o

  let to_float v =
    if Js.typeof v == Js.string "number" then
      Js.Unsafe.coerce v |> Js.float_of_number |> Option.return
    else
      None

  let to_int v =
    let+ v = to_float v in
    Int.of_float v

  let to_int64 v =
    let+ v = to_float v in
    Int64.of_float v

  let to_json v = Impl_js.of_js v

  let to_list v =
    if Js.instanceof v Js.array_empty then
      Js.Unsafe.coerce v |> Js.to_array |> Array.to_list |> Option.return
    else
      None

  let to_string v =
    if Js.typeof v == Js.string "string" then
      Js.Unsafe.coerce v |> Js.to_string |> Option.return
    else
      None

  let to_time v =
    if Js.instanceof v Js.date_now then
      let date = Js.Unsafe.coerce v in
      Ptime.of_float_s (date##getTime /. 1000.)
      |> Result.of_option ~error:"invalid date"
    else
      Result.fail @@ Schematic.Coding.type_error "Date" (type_name v)

  let to_time_local v =
    Result.map (to_time v) ~f:(fun time -> (time, 0 (* FIXME *)))
end

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

include Schematic.Coding.Make (Jsoo_coding)

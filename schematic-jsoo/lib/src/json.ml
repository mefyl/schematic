open Base
module T = Schematic.Json.Make (Impl_js)
include T

module String = Schematic.Coding.Make_string (struct
  include Schematic.Coding.Make (struct
    include Jsoo.Impl

    let of_time t =
      Ptime.to_rfc3339 t
      |> Js_of_ocaml.Js.string
      |> Js_of_ocaml.Js.Unsafe.coerce

    let to_time v =
      match to_string v with
      | None -> Result.fail None
      | Some string -> Schematic.Json.Impl.to_time (`String string)

    let of_time_local (t, tz_offset_s) =
      Ptime.to_rfc3339 ~tz_offset_s t
      |> Js_of_ocaml.Js.string
      |> Js_of_ocaml.Js.Unsafe.coerce

    let to_time_local v =
      match to_string v with
      | None -> Result.fail None
      | Some string -> Schematic.Json.Impl.to_time_local (`String string)

    let of_date = `Generic
    let to_date = `Generic
  end)

  let content_type = "application/json"

  let of_string s =
    try
      Js_of_ocaml.Js.Unsafe.global##._JSON##parse (Js_of_ocaml.Js.string s)
      |> Result.return
    with Js_of_ocaml.Js_error.Exn e ->
      Result.fail @@ Js_of_ocaml.Js_error.message e

  let to_string v =
    Js_of_ocaml.Js.Unsafe.global##._JSON##stringify v
    |> Js_of_ocaml.Js.to_string
end)

let of_js = Impl_js.of_js
let to_js = Impl_js.to_js
let schema = Schematic.Json.schema

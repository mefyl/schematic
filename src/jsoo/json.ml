open Base
open Js_of_ocaml

let ( == ) = Caml.( == )

let _Array = Js.Unsafe.global##._Array

module Impl_js = struct
  let rec of_js (o : Js.Unsafe.any) =
    let o = Js.Unsafe.coerce o in
    if o == Js.Unsafe.pure_js_expr "null" then
      `Null
    else if Js.typeof o == Js.string "string" then
      `String (Js.to_string o)
    else if Js.typeof o == Js.string "number" then
      `Float (Js.float_of_number o)
    else if Js.typeof o == Js.string "boolean" then
      `Bool (Js.to_bool o)
    else if (Js.Unsafe.pure_js_expr "Array")##isArray o then
      `A (Js.array_map of_js o |> Js.to_array |> Array.to_list)
    else
      let entries =
        let keys = Js.object_keys o |> Js.to_array
        and f key = (Js.to_string key, of_js @@ Js.Unsafe.get o key) in
        Array.map ~f keys |> Array.to_list
      in
      `O entries

  let rec to_js x =
    match x with
    | `Null -> Js.Unsafe.pure_js_expr "null"
    | `String s -> Js.string s |> Js.Unsafe.coerce
    | `Float n -> Js.number_of_float n |> Js.Unsafe.coerce
    | `Bool n -> Js.bool n |> Js.Unsafe.coerce
    | `A a -> List.map ~f:to_js a |> List.to_array |> Js.array
    | `O o ->
      let res = object%js end in
      let f (k, v) = Js.Unsafe.set res (Js.string k) (to_js v) in
      let () = List.iter ~f o in
      res |> Js.Unsafe.coerce

  let of_string s =
    try
      let json = Js._JSON##parse (Js.string s) in
      Result.return @@ of_js json
    with _ -> Result.fail @@ `Parse "JSON parse error"

  let to_string t = to_js t |> Js_of_ocaml.Json.output |> Js.to_string
end

include Schematic.Json.Make (Impl_js)

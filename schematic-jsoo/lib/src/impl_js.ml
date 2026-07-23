open Base
open Js_of_ocaml

let ( == ) = Stdlib.( == )

let of_js =
  let const_boolean = Jstr.v "boolean"
  and const_number = Jstr.v "number"
  and const_string = Jstr.v "string"
  and object' = Jv.get Jv.global "Object" in
  let rec of_js o =
    if Jv.is_none o then `Null
    else if Jv.is_array o then `A (Jv.to_list of_js o)
    else
      let type' = Jv.typeof o in
      if Jstr.equal type' const_string then `String (Jv.to_string o)
      else if Jstr.equal type' const_number then `Float (Jv.to_float o)
      else if Jstr.equal type' const_boolean then `Bool (Jv.to_bool o)
      else
        let keys = Jv.to_list Jv.to_jstr @@ Jv.call object' "keys" [| o |] in
        let f key = (Jstr.to_string key, of_js @@ Jv.get' o key) in
        `O (List.map ~f keys)
  in
  fun (o : Js.Unsafe.any) -> of_js (Jv.repr o)

let rec to_js x =
  match x with
  | `Null -> Js.Unsafe.pure_js_expr "null"
  | `String s -> Js.string s |> Js.Unsafe.coerce
  | `Float n -> Js.number_of_float n |> Js.Unsafe.coerce
  | `Bool n -> Js.bool n |> Js.Unsafe.coerce
  | `A a -> List.map ~f:to_js a |> List.to_array |> Js.array |> Js.Unsafe.coerce
  | `O o ->
    let res = object%js end in
    let f (k, v) = Js.Unsafe.set res (Js.string k) (to_js v) in
    let () = List.iter ~f o in
    res |> Js.Unsafe.coerce

let of_string s =
  try
    let js = Js._JSON##parse (Js.string s) in
    Result.return @@ of_js js
  with _ -> Result.fail "parse error"

let to_string t =
  let js = to_js t in
  let json = Js_of_ocaml.Json.output js in
  Js.to_string json

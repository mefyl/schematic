open Base
open Js_of_ocaml

type o = {
  i : int;
  s : string;
  b : bool;
  opt : bool option;
  a : int list;
}
[@@deriving schema]

type js =
  < i : int Js.readonly_prop
  ; s : Js.js_string Js.t Js.readonly_prop
  ; b : bool Js.t Js.readonly_prop
  ; opt : bool Js.t Js.Opt.t Js.readonly_prop
  ; a : int Js.js_array Js.t Js.readonly_prop >
  Js.t

let fail s = Js.raise_js_error @@ new%js Js.error_constr (Js.string s)

let of_result = function
  | Result.Ok v -> v
  | Result.Error (`Parse e) -> fail ("parse error: " ^ e)
  | Result.Error (`Json_decode e) -> fail ("decode error: " ^ e)

let to_js { i; s; b; opt; a } : js =
  object%js
    val i = i

    val s = Js.string s

    val b = Js.bool b

    val opt = Option.map ~f:Js.bool opt |> Js.Opt.option

    val a = List.to_array a |> Js.array
  end

let of_js (o : js) =
  {
    i = o##.i;
    s = o##.s |> Js.to_string;
    b = o##.b |> Js.to_bool;
    opt = Js.Opt.to_option o##.opt |> Option.map ~f:Js.to_bool;
    a = Js.to_array o##.a |> Array.to_list;
  }

let () =
  Js.export_all
    (object%js
       method ofString s =
         of_result
         @@
         let open Result in
         Schematic_jsoo.Json.of_string (Js.to_string s) >>= fun json ->
         Schematic.Json.decode o_schema json >>| to_js

       method toString o =
         of_js o
         |> Schematic.Json.encode o_schema
         |> Schematic_jsoo.Json.to_string |> Js.string
    end)

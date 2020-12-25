open Base
open Js_of_ocaml

module Namespace = struct
  type int_alias = int [@@deriving schema ~jsoo]
end

type o = {
  i : int;
  s : string;
  b : bool;
  opt : bool option;
  a : Namespace.int_alias list;
  pair : bool * bool;
}

and variant =
  | This
  | That

and poly =
  [ `This
  | `That
  ]
[@@deriving schema ~jsoo]

let fail s = Js.raise_js_error @@ new%js Js.error_constr (Js.string s)

let of_result = function
  | Result.Ok v -> v
  | Result.Error (`Parse e) -> fail ("parse error: " ^ e)
  | Result.Error (`Json_decode e) -> fail ("decode error: " ^ e)

let () =
  Js.export_all
    (object%js
       method ofString s =
         of_result
         @@
         let open Result in
         Schematic_jsoo.Json.of_string (Js.to_string s) >>= fun json ->
         Schematic.Json.decode o_schema json >>| o_to_js

       method toString o =
         o_of_js_exn o
         |> Schematic.Json.encode o_schema
         |> Schematic_jsoo.Json.to_string |> Js.string
    end)

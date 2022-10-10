open Base
open Js_of_ocaml

module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

module Namespace = struct
  type int_alias = int [@@deriving schema ~jsoo]
end

type 'param inline = {
  s : 'param;
  b : bool;
}
[@@deriving schema ~jsoo]

type o = {
  i : int;
  inline : string inline; [@schematic.inline]
  opt : bool option;
  opt_nullable : bool option option;
  a : Namespace.int_alias list;
  pair : bool * bool;
  def : int; [@default 0]
}

and variant =
  | This
  | That of int

and poly =
  [ `This
  | `That
  ]
[@@deriving schema ~jsoo]

type keyed =
  | Key_a of { i : int }
  | Key_b of { s : string }
[@@schematic.union_key "type_key"] [@@deriving schema ~jsoo]

let of_result = function
  | Result.Ok v -> v
  | Result.Error e ->
    Js_error.raise_ @@ Js_error.of_error @@ new%js Js.error_constr (Js.string e)

let decode_error coding =
  Result.map_error ~f:(fun e ->
      Fmt.str "%s decode error: %a" coding Schematic.Error.pp_decoding e)

(** Check inlining works in the expanded JSOO type *)
let f (o : o_js) =
  let (_ : Js.js_string Js.t) = o##.s
  and (_ : bool Js.t Js.Optdef.t) = o##.opt
  and (_ : bool Js.t Js.Opt.t Js.Optdef.t) = o##.opt_nullable
  and (_ : Js.number Js.t Js.Optdef.t) = o##.def in
  ()

let g (o : keyed_js) =
  let (_ : Js.js_string Js.t) = o##.type_key in
  ()

let () =
  Js.export_all
    (object%js
       method ofString s =
         of_result
         @@
         let open Result in
         Schematic_jsoo.Json.of_string (Js.to_string s) >>= fun json ->
         Schematic.Json.decode o_schema json |> decode_error "JSON" >>| o_to_js

       method ofJs v =
         of_result
         @@
         let open Result in
         Schematic_jsoo.Jsoo.decode o_schema v |> decode_error "js" >>| o_to_js

       method toString o =
         o_of_js_exn o
         |> Schematic.Json.encode o_schema
         |> Schematic_jsoo.Json.to_string |> Js.string

       method toJs o = o_of_js_exn o |> Schematic_jsoo.Jsoo.encode o_schema

       method variantRoundtrip (js : variant_js) =
         of_result
         @@
         let open Result in
         Schematic_jsoo.Jsoo.decode variant_schema js
         |> decode_error "js" >>| variant_to_js
    end)

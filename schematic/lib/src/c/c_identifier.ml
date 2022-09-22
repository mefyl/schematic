open Base

type t = Id of string [@@deriving eq, ord]

let make s =
  let s = String.split ~on:'-' s |> String.concat ~sep:"_" in

  (* TODO: normalize here *)
  Id s

let map ~f = function Id s -> Id (f s)
let to_string = function Id s -> s
let pp f id = Fmt.string f (to_string id)

open Base

type t = Id of string [@@deriving eq, ord, hash]

let reserved_typenames = [ "time"; "id" ]

let make s =
  if List.mem reserved_typenames s ~equal:String.equal then Id (s ^ "_")
  else
    let s = String.split ~on:'-' s |> String.concat ~sep:"_" in
    Id s

let map ~f = function Id s -> Id (f s)
let to_string = function Id s -> s
let pp f id = Fmt.string f (to_string id)

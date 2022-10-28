open Base

type t = Id of string [@@deriving eq, ord, hash]

(* Common list for types abd bindings. TODO: distinguish the two *)
let reserved_names = [ "time"; "id"; "self" ]

(* This is assumed to be idempotent. *)
let make s =
  if List.mem reserved_names s ~equal:String.equal then Id (s ^ "_")
  else
    let s = String.split ~on:'-' s |> String.concat ~sep:"_" in
    Id s

let equal (Id l) (Id r) = String.equal l r
let compare (Id l) (Id r) = String.compare l r
let map ~f = function Id s -> Id (f s)
let to_string = function Id s -> s
let pp f id = Fmt.string f (to_string id)
let normalize_s s = s |> make |> to_string

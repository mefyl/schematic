open Base

(* TODO: use a single Identifier type to convert to all needed styles /
   encodings... *)
type t = Id of string [@@deriving eq, ord, hash]

(* Common list for types and bindings. TODO: distinguish the two *)
let reserved_names = [ "time"; "id"; "self" ]

(* This is assumed to be idempotent. *)
(* XXX(perf): this is a very naive implementation. *)
let make s =
  if List.mem reserved_names s ~equal:String.equal then Id (s ^ "_")
  else
    let s =
      String.split ~on:'-' s
      |> String.concat ~sep:"_"
      |> String.substr_replace_all ~pattern:"'" ~with_:"_sq_"
      |> String.substr_replace_all ~pattern:"\"" ~with_:"_dblq_"
    in
    Id s

let map ~f = function Id s -> Id (f s)
let to_string = function Id s -> s
let pp f id = Fmt.string f (to_string id)
let normalize_s s = s |> make |> to_string

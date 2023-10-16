open Base

(* TODO: use a single Identifier type to convert to all needed styles /
   encodings... *)
type t = string [@@deriving eq, ord, hash]

(* Common list for types and bindings. TODO: distinguish the two *)
let reserved_names = [ "time"; "id"; "self" ]

(* This is assumed to be idempotent. *)
(* XXX(perf): this is a very naive implementation. *)
let make s =
  if List.mem reserved_names s ~equal:String.equal then s ^ "_"
  else
    String.split ~on:'-' s |> String.concat ~sep:"_"
    |> String.substr_replace_all ~pattern:"'" ~with_:"_sq_"
    |> String.substr_replace_all ~pattern:"\"" ~with_:"_dblq_"

let map ~f s = f s
let to_string s = s
let pp = Fmt.string
let normalize_s s = s |> make

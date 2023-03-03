module Entropy = struct
  type t = Cryptokit.Random.rng

  let get rng len =
    let buf = Bytes.create len in
    let () = rng#random_bytes buf 0 len in
    (rng, Some buf)
end

module Entropic = Entropic.Make (Entropy)

let entropy =
  Cryptokit.Random.pseudo_rng @@ Printf.sprintf "%.16f" @@ Unix.gettimeofday ()

let () =
  match Entropic.generate entropy Schematic.Schemas.json_schema with
  | Result.Error e ->
    Fmt.epr "%s@." e;
    exit 1
  | Result.Ok json -> Fmt.pr "%a@." Schematic.Json.pp json

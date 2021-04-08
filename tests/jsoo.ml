(* Check generating Jsoo binding does not prevent running natively *)

type jsoo = { jsoo : int } [@@deriving schema ~jsoo]

let () = Fmt.pr "%a" Schematic.Schema.pp jsoo_schema

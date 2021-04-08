(* Check generating Jsoo binding does not prevent running natively *)

type jsoo = { jsoo : int } [@@deriving schema ~jsoo:false]

let () = Fmt.pr "%a" Schematic.Schema.pp jsoo_schema

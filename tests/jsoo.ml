(* Check generating Jsoo binding does not prevent running natively *)

module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

type jsoo = { jsoo : int } [@@deriving schema ~jsoo:false]

let () = Fmt.pr "%a" Schematic.Schema.pp jsoo_schema

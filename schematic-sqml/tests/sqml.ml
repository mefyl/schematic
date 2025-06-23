module Schemas_library = struct
  type Schematic.Schema.library += V

  let v = V
end

module Model = struct
  include Sqml.Model.Make (struct
    let version = Schematic.Version.make 0 0 0
  end)
end

type t = {
  id : int; [@schematic.sqml.primary]
  name : string; [@schematic.sqml.type Varchar 16]
  description : string option;
}
[@@deriving schema ~plugins:{ sqml = "entries" }]

module Entries = Sqml_meta_entries.Import (Model.BindTable (Sqml_meta_entries))

let (_ : int Entries.Column.t) = Entries.Column.id
let (_ : string Entries.Column.t) = Entries.Column.name
let (_ : string option Entries.Column.t) = Entries.Column.description
let (_ : int Entries.Expression.t) = Entries.id
let (_ : string Entries.Expression.t) = Entries.name
let (_ : string option Entries.Expression.t) = Entries.description

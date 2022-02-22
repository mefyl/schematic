include Semver

let make major minor patch =
  Base.Option.value_exn ~here:[%here]
    (Semver.from_parts major minor patch [] [])

module Option = struct
  module T = struct
    type t = Semver.t option [@@deriving ord]

    let sexp_of_t = function
      | None -> Base.Sexp.List []
      | Some { major; minor; patch; _ } ->
        Base.Sexp.(
          List
            [
              Atom (Int.to_string major);
              Atom (Int.to_string minor);
              Atom (Int.to_string patch);
            ])
  end

  include T
  include Base.Comparable.Make (T)
end

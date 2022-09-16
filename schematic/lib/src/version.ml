include Semver

let sexp_of_t = function
  | { major; minor; patch; _ } ->
    Base.Sexp.(
      List
        [
          Atom (Int.to_string major);
          Atom (Int.to_string minor);
          Atom (Int.to_string patch);
        ])

let make ?(prerelease = []) ?(build = []) major minor patch =
  Base.Option.value_exn ~here:[%here]
    (Semver.from_parts major minor patch prerelease build)

include Base.Comparable.Make (struct
  include Semver

  let sexp_of_t = sexp_of_t
end)

module Option = struct
  module T = struct
    type t = Semver.t option [@@deriving ord]

    let sexp_of_t = function
      | None -> Base.Sexp.List []
      | Some version -> sexp_of_t version
  end

  include T
  include Base.Comparable.Make (T)
end

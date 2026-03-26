include Semver

let of_string_exn s =
  match of_string s with
  | Some version -> version
  | None -> Fmt.failwith "invalid semantic version: %S" s

let sexp_of_t = function
  | { major; minor; patch; prerelease; build } ->
    Base.Sexp.(
      List
        [
          Atom (Int.to_string major);
          Atom (Int.to_string minor);
          Atom (Int.to_string patch);
          List (List.map (fun v -> Base.Sexp.Atom v) prerelease);
          List (List.map (fun v -> Base.Sexp.Atom v) build);
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

let schema =
  let decode s =
    match of_string s with
    | Some version -> Result.Ok version
    | None -> Result.Error (Fmt.str "invalid semantic version: %s" s)
  in
  Schema.map ~decode ~encode:to_string @@ Schemas.string_schema

let schema_versioned _ = schema

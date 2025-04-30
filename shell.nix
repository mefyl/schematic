let
  logisticPath =
    if builtins.pathExists ./path-to-logistic.nix
    then (import ./path-to-logistic.nix) else ./.logistic;

  logisticFullNixShell = import "${logisticPath}/nix/full-nix-shell.nix";

  thisRoutineRepo = logisticFullNixShell.lib.mkRoutineRepo ./. {
    withSwift = true;

    # Explicitly ignore some opam files because they depend on `timmy` which depends on `schematic`, which is not really supported by `opam-nix` (and frankly is a weird code organization)
    # See https://github.com/tweag/opam-nix/issues/111
    opamFilesRegexes =
      [
        "^schematic.opam$"
        "^schematic-cli.opam$"
        "^schematic-ffi.opam$"
        "^schematic-jsoo.opam$"
        "^schematic-sexp.opam$"
      ];
  };
in
thisRoutineRepo.devShells.${builtins.currentSystem}.default

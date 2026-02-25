let
  logisticPath =
    if builtins.pathExists ./path-to-logistic.nix
    then (import ./path-to-logistic.nix) else ./.logistic;

  shell = import "${logisticPath}/nix/shell.nix"
    {
      extraPackages = pkgs: [ ];
    };
in
shell

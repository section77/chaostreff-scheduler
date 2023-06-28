let

  pkgs = import (builtins.fetchGit {
    name = "nixos-23.05";
    url = "https://github.com/nixos/nixpkgs/";
    ref = "refs/heads/nixos-23.05";
    rev = "9790f3242da2152d5aa1976e3e4b8b414f4dd206";
    }) {};

  drv = pkgs.haskellPackages.developPackage {
    root = ./.;
  };
in
if pkgs.lib.inNixShell then drv.env else drv

let

  pkgs = import (builtins.fetchGit {
    name = "nixos-23.11";
    url = "https://github.com/nixos/nixpkgs/";
    ref = "refs/heads/nixos-23.11";
    rev = "a77ab169a83a4175169d78684ddd2e54486ac651";
    }) {};

  drv = pkgs.haskellPackages.developPackage {
    root = ./.;
  };
in
if pkgs.lib.inNixShell then drv.env else drv

let
  pkgs = import (builtins.fetchGit {
    name = "nixos-24.11";
    url = "https://github.com/nixos/nixpkgs/";
    ref = "refs/heads/nixos-24.11";
    rev = "8b27c1239e5c421a2bbc2c65d52e4a6fbf2ff296";
  }) {};

  drv = pkgs.haskellPackages.developPackage {
    root = ./.;
  };
in
  if pkgs.lib.inNixShell
  then drv.env
  else drv

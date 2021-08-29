let

  pkgs = import (builtins.fetchGit {
    name = "nixos-21.05";
    url = "https://github.com/nixos/nixpkgs/";
    ref = "refs/heads/nixos-21.05";
    rev = "a1007637cea374bd1bafd754cfd5388894c49129";
    }) {};

  drv = pkgs.haskellPackages.callPackage ./chaostreff-scheduler.nix {};
in
if pkgs.lib.inNixShell then drv.env else drv

let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz";
    sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  }) {};

  drv = pkgs.haskellPackages.callPackage ./chaostreff-scheduler.nix {};
in
if pkgs.lib.inNixShell then drv.env else drv

{ pkgs ? import <nixpkgs> {} }:

let
  stdenv = pkgs.stdenv;
  eggs = import ./eggs.nix { inherit pkgs stdenv; };
in
stdenv.mkDerivation {
  name = "chaostreff-scheduler";
  src = ./.;


  buildInputs = with pkgs; [
    chicken
    egg2nix
    rlwrap
    glibc.static
  ] ++ (with stdenv.lib; filter isDerivation (attrValues eggs));


  buildPhase = ''
    csc -deploy chaostreff-scheduler.scm
  '';

  installPhase = ''
    mkdir -p $out

    work=$TMP/chaostreff-scheduler

    chicken-install -retrieve matchable > /dev/null
    cd matchable && csc -unit matchable -emit-import-library matchable -c matchable.scm -o $work/matchable.o

    cd $src && csc -uses matchable -I $work/matchable -c chaostreff-scheduler.scm -o $work/chaostreff-scheduler.o
    csc -static $work/chaostreff-scheduler.o $work/matchable.o -o $out/chaostreff-scheduler
  '';
}

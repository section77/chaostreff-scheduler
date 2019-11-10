{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, directory, doctest, filepath, hpack
      , megaparsec, optparse-applicative, process, protolude, stdenv
      , text, time, unix
      }:
      mkDerivation {
        pname = "chaostreff-scheduler";
        version = "0.0.3";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          base directory filepath megaparsec optparse-applicative process
          protolude text time unix
        ];
        testHaskellDepends = [
          base directory doctest filepath megaparsec optparse-applicative
          process protolude text time unix
        ];
        preConfigure = "hpack";
        description = "schedule chaostreff events";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv

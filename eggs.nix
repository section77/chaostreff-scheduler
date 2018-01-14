{ pkgs, stdenv }:
rec {
  inherit (pkgs) eggDerivation fetchegg;

  matchable = eggDerivation {
    name = "matchable-3.7";

    src = fetchegg {
      name = "matchable";
      version = "3.7";
      sha256 = "1vc9rpb44fhn0n91hzglin986dw9zj87fikvfrd7j308z22a41yh";
    };

    buildInputs = [
      
    ];
  };
}


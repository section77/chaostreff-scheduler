let
  stdenv = pkgs.stdenv;
  eggs = import ./eggs.nix { inherit pkgs stdenv; };
  config = rec {
    packageOverrides = pkgs: rec {
      chaostreff-scheduler = pkgs.callPackage ./default.nix {};

      container = pkgs.dockerTools.buildImage {
        name = "chaostreff-scheduler";
	contents = [ chaostreff-scheduler pkgs.busybox ];
	config = {
	  Entrypoint = "/bin/chaostreff-scheduler";
	  Cmd = [ "" ];
	  Volumes = {
	    "/website" = {};
	  };
	};
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; system = "x86_64-linux"; };
in { chaostreff-scheduler = pkgs.chaostreff-scheduler;
     container = pkgs.container;
   }

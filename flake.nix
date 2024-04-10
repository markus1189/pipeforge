{
  description = "A very basic flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        hsPkgs = pkgs.haskellPackages;

        appDrv = pkgs.haskell.lib.justStaticExecutables
          (hsPkgs.callCabal2nix "pipeforge" ./. { });

        devEnv = hsPkgs.developPackage {
          returnShellEnv = true;
          root = ./.;
          modifier = with pkgs.haskell.lib;
            drv:
            dontHaddock (disableOptimization (disableLibraryProfiling drv));
        };
      in rec {
        apps.pipeforge = {
          type = "app";
          program = "${pkgs.haskell.lib.justStaticExecutables appDrv}/bin/pipeforge";
        };
        defaultApp = apps.pipeforge;

        packages.pipeforge = appDrv;
        defaultPackage = packages.pipeforge;

        devShell = pkgs.mkShell {
          inputsFrom = [ devEnv ];
          buildInputs = with hsPkgs; [ haskell-language-server implicit-hie ];
        };
      });
}

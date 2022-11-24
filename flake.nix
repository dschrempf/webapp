{
  description = "Haskell development environment";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  inputs.mcmc.url = "github:dschrempf/mcmc";
  inputs.mcmc.inputs.nixpkgs.follows = "nixpkgs";

  outputs =
    { self
    , flake-utils
    , mcmc
    , nixpkgs
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        haskellPackageNames = [
          "webapp"
        ];
        ghcVersion = "ghc924";
        haskellMkPackage = hps: nm: hps.callCabal2nix nm (./. + "/${nm}") { };
        haskellOverlay = (
          selfn: supern: {
            haskellPackages = supern.haskell.packages.${ghcVersion}.override {
              overrides = selfh: superh:
                {
                  hvega = selfh.callCabal2nix "hvega" ./modules/hvega rec { };
                  mcmc = mcmc.packages.${system}.default;
                  webapp = selfh.callCabal2nix "webapp" ./. rec { };
                };
            };
          }
        );
        overlays = [ haskellOverlay ];
        pkgs = import nixpkgs {
          inherit system;
          inherit overlays;
        };
        hpkgs = pkgs.haskellPackages;
        webappPkgs = nixpkgs.lib.genAttrs haskellPackageNames (n: hpkgs.${n});
        webappPkgsDev = builtins.mapAttrs (_: x: pkgs.haskell.lib.doBenchmark x) webappPkgs;
      in
      {
        packages = webappPkgs // { webappPkgs.default = webappPkgs.webapp; };

        devShells.default = hpkgs.shellFor {
          # shellHook =
          #   let
          #     scripts = ./scripts;
          #   in
          #   ''
          #     export PATH="${scripts}:$PATH"
          #   '';
          packages = _: (builtins.attrValues webappPkgsDev);
          nativeBuildInputs = with pkgs; [
            # See https://github.com/NixOS/nixpkgs/issues/59209.
            bashInteractive

            # Haskell toolchain.
            hpkgs.cabal-fmt
            hpkgs.cabal-install
            hpkgs.haskell-language-server
          ];
          buildInputs = with pkgs; [
          ];
          doBenchmark = true;
          # withHoogle = true;
        };
      }
    );
}

{
  description = "Haskell development environment";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.hvega.url = "github:dschrempf/hvega/dom";
  inputs.hvega.inputs.nixpkgs.follows = "nixpkgs";

  inputs.mcmc.url = "github:dschrempf/mcmc";
  inputs.mcmc.inputs.nixpkgs.follows = "nixpkgs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self
    , flake-utils
    , hvega
    , mcmc
    , nixpkgs
    }:
    let
      theseHpkgNames = [
        "webapp"
      ];
      thisGhcVersion = "ghc96";
      hOverlay = selfn: supern: {
        haskell = supern.haskell // {
          packageOverrides = selfh: superh:
            supern.haskell.packageOverrides selfh superh //
              {
                webapp = selfh.callCabal2nix "webapp" ./. { };
              };
        };
      };
      overlays = [
        hOverlay
        hvega.overlays.default
        mcmc.overlays.default
      ];
      perSystem = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            inherit overlays;
          };
          hpkgs = pkgs.haskell.packages.${thisGhcVersion};
          hlib = pkgs.haskell.lib;
          theseHpkgs = nixpkgs.lib.genAttrs theseHpkgNames (n: hlib.dontCheck hpkgs.${n});
          theseHpkgsDev = builtins.mapAttrs
            (_: x: hlib.doCheck
              (hlib.doBenchmark x))
            theseHpkgs;
        in
        {
          packages = theseHpkgs // { default = theseHpkgs.webapp; };

          devShells.default = hpkgs.shellFor {
            packages = _: (builtins.attrValues theseHpkgsDev);
            nativeBuildInputs = [
              # Haskell toolchain.
              hpkgs.cabal-fmt
              hpkgs.cabal-install
              hpkgs.haskell-language-server
            ];
            buildInputs = [
            ];
            doBenchmark = true;
            withHoogle = true;
          };
        };
    in
    {
      overlays.default = nixpkgs.lib.composeManyExtensions overlays;
      nixosModules = let webapp = import ./modules/webapp.nix self; in
        {
          inherit webapp;
          default = webapp;
        };
    }
    // flake-utils.lib.eachDefaultSystem perSystem;
}

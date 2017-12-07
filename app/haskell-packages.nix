{ pkgs ? import <nixpkgs> {} }:

rec {
  haskellPackages = pkgs.haskell.packages.ghc802.override {
    overrides =
      self: super: {
          # https://github.com/folsen/opaleye-gen/issues/8
          opaleye-gen = haskellPackageGen { doFilter = false; } (
            pkgs.fetchFromGitHub {
              owner = "folsen";
              repo = "opaleye-gen";
              rev = "14938df0081187539f23f8547fb1b7762e286ac3";
              sha256 = "1xapgyhkn71m0arb06rv5b1cncz5gv9lybi3q4yavs8zh4jbkbn7";
            }
          );
          # Has a better function for updating tables
          opaleye = pkgs.haskell.lib.dontCheck super.opaleye_0_6_0_0;

          # May as well keep up with servant api churn
          servant = super.servant_0_12;
          servant-server = super.servant-server_0_12;
          servant-client = super.servant-client_0_12_0_1;
          servant-foreign = super.servant-foreign_0_10_2;
        };
      };

  # haskellPackageGen takes some options and a source location and generates a
  # derivation which builds the haskell package at that source location.
  haskellPackageGen = { doFilter ? true
                      , doHaddock ? true
                      , extraEnvPackages ? [] # Any extra packages to be made available in the developer shell only
                      }: src:
    let filteredSrc = if builtins.typeOf src != "path" then src else
          builtins.filterSource (path: type:
          type != "unknown" &&
          (baseNameOf path == "dist" -> type != "directory")
        ) src;
        package = pkgs.runCommand "default.nix" {} ''
          ${pkgs.haskell.packages.ghc802.cabal2nix}/bin/cabal2nix \
            ${if doFilter then filteredSrc else src} \
            ${if doHaddock then "" else "--no-haddock"} \
            > $out
        '';

        drv = haskellPackages.callPackage package {};

        envWithExtras = pkgs.lib.overrideDerivation drv.env (attrs: {
          buildInputs = attrs.buildInputs ++ extraEnvPackages;
        });
    in drv // { env = envWithExtras; };
}

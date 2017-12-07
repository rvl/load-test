{ pkgs ? import <nixpkgs> {}, src ? ./. }:

with (import ./haskell-packages.nix) {inherit pkgs;};
with haskellPackages; 
with pkgs;

let filterHaskell = builtins.filterSource (path: type: ((builtins.match ".*\.(hs|cabal)$" path != null) || type == "directory") && baseNameOf (toString path) != "dist");

in haskellPackageGen {
  extraEnvPackages = [ opaleye-gen postgresql ];
} (filterHaskell src)

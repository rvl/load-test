{ pkgs ? import <nixpkgs> {}, src ? ./. }:

with (import ./haskell-packages.nix) {inherit pkgs;};
with haskellPackages; 
with pkgs;

haskellPackageGen {
  extraEnvPackages = [ opaleye-gen postgresql ];
} src

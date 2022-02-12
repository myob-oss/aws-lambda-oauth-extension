{pkgs ? import ./nixpkgs.nix, haskellPackage ? pkgs.haskell.packages.ghc8107 }:
haskellPackage.callCabal2nix "aws-lambda-oauth-extension" ./. {}

{pkgs ? import ./nixpkgs.nix, haskellPackage ? pkgs.haskell.packages.ghc8104 }:
haskellPackage.callCabal2nix "aws-lambda-oauth-extension" ./. {}

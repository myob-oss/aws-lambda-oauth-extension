{pkgs ? import ./nixpkgs.nix, haskellPackage ? pkgs.haskell.packages.ghc8107 }:
let aws-lambda-oauth-extension-cabal = haskellPackage.callCabal2nix "aws-lambda-oauth-extension" ./. {};
in {
  inherit aws-lambda-oauth-extension-cabal;
  aws-lambda-oauth-extension = pkgs.haskell.lib.justStaticExecutables aws-lambda-oauth-extension-cabal;
}

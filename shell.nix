# pin to latest commit of nixos 21.05 https://status.nixos.org/
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/4181644d09b9.tar.gz") {} }:
with pkgs;

haskell.lib.buildStackProject {
  name = "aws-lambda-oauth-extension";
  src = ./.;
  buildInputs = [
    zlib
    dhall-bash
    dhall-json
    bats
  ];
}

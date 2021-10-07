with (import ./nixpkgs.nix);

haskell.lib.buildStackProject {
  name = "aws-lambda-oauth-extension-env";
  src = ./.;
  buildInputs = [
    zlib
    dhall-bash
    dhall-json
    bats
  ];
}

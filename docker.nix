let pkgs = import ./nixpkgs.nix;
    binary = pkgs.haskell.lib.justStaticExecutables (import ./default.nix {});
in pkgs.dockerTools.streamLayeredImage {
  name = "ghcr.io/myob-oss/aws-lambda-oauth-extension";
  tag = "latest";
  created = "now";
  contents = [binary pkgs.cacert];
  config = {
    Entrypoint = [ "/bin/aws-lambda-oauth-extension-exe"];
  };
}

let pkgs = import ./nixpkgs.nix;
    binary = pkgs.haskell.lib.justStaticExecutables (import ./default.nix {});
    extensionAlias = pkgs.runCommand "symlink" {} ''
      mkdir -p $out/opt/extensions
      ln -s ${binary}/bin/aws-lambda-oauth-extension-exe $out/opt/extensions/aws-lambda-oauth-extension
      '';
in pkgs.dockerTools.streamLayeredImage {
  name = "ghcr.io/myob-oss/aws-lambda-oauth-extension";
  tag = "latest";
  created = "now";
  contents = [binary pkgs.cacert extensionAlias];
  config = {
    Entrypoint = [ "/opt/extensions/aws-lambda-oauth-extension"];
  };
}

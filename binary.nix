let pkgs = import ./nixpkgs.nix;
    source = import ./default.nix {};
in pkgs.haskell.lib.justStaticExecutables source

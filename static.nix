with (import ./nixpkgs.nix);

haskell.lib.appendConfigureFlags
  (haskell.lib.justStaticExecutables (pkgs.haskell.lib.dontCheck (import ./default.nix {
    pkgs = pkgsMusl;
    haskellPackage = pkgsMusl.haskell.packages.ghc8107.override (old: {
      overrides = let
                extension =
                  haskellPackagesNew: haskellPackagesOld: {
                    time-compat = pkgsMusl.haskell.lib.dontCheck haskellPackagesOld.time-compat;
                  };
              in
                pkgsMusl.lib.composeExtensions
                  (old.overrides or (_: _: {}))
                  extension;
    });
  }).aws-lambda-oauth-extension-cabal))
  [ "--enable-executable-static"
    "--extra-lib-dirs=${pkgsMusl.ncurses.override { enableStatic = true; }}/lib"
    "--extra-lib-dirs=${pkgsMusl.gmp6.override { withStatic = true; }}/lib"
    "--extra-lib-dirs=${pkgsMusl.zlib.static}/lib"
    "--extra-lib-dirs=${pkgsMusl.libsodium.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
    "--extra-lib-dirs=${pkgsMusl.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
  ]

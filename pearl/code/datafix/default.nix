let nixpkgs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz) {};
    compiler = "ghc8102";
in
nixpkgs.haskell.lib.doBenchmark ((nixpkgs.pkgs.haskell.packages.${compiler}.override {
  overrides = self: super: {
    pomaps = nixpkgs.haskell.lib.doJailbreak super.pomaps;
    # src = ../cabal-toolkit;
  };
}).callPackage ./datafix.nix { })

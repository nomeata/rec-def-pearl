with import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/5ff268a184fa41ee6c9cf337975fbdec62f8481e.tar.gz") {};
stdenv.mkDerivation rec {
  name = "env";
  buildInputs = [
    (ghc.withPackages(p : [p.rec-def]))
  ];
}

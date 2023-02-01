with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";
  buildInputs = [ lhs2tex ];
}

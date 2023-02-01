with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";
  buildInputs = [
    (lhs2tex.overrideAttrs( old : {
      src = fetchFromGitHub {
         owner = "nomeata";
         repo = "lhs2tex";
         # ref = "mychanges";
         rev = "e2ed3a9ac606869a8e7ed49533c96dab2a0e2cdd";
         hash = "sha256-b1hH+Wrrqb4Xxi7CXGNeQUo286Lil1Ea9+0ZYVbwe6M=";
       }
    ;}))
  ];
}

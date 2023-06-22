#!/usr/bin/env bash

set -e

version=0.2.1

cd ..
nix-shell --run "cabal sdist"
cd pearl || exit
rm -rf rec-def-$version
aunpack ../dist-newstyle/sdist/rec-def-$version.tar.gz

sed -i -e "s,Joachim Breitner,The Author," rec-def-$version/LICENSE rec-def-$version/rec-def.cabal
sed -i -e "s,joachim-breitner.de,author.example," rec-def-$version/LICENSE rec-def-$version/rec-def.cabal
sed -i -e "s,nomeata,author," rec-def-$version/LICENSE rec-def-$version/rec-def.cabal

rm -f rec-def-$version-anonym.tar.gz
apack rec-def-$version-anonym.tar.gz rec-def-$version/
rm -rf rec-def-$version
echo "Created rec-def-$version-anonym.tar.gz"

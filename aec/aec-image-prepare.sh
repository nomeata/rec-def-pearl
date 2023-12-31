#!/usr/bin/env bash

set -e

cabal update
cd /home/artifact
rm -rf rec-def-0.2.1
tar xavf rec-def-0.2.1.tar.gz
cd rec-def-0.2.1
cabal build --write-ghc-environment-files=always
cabal test
cabal haddock --haddock-hyperlink-source --haddock-quickjump --haddock-html-location='https://hackage.haskell.org/package/$pkg-$version/doc'
cd /home/artifact
mv /home/artifact/rec-def-0.2.1/dist-newstyle/build/x86_64-linux/ghc-8.8.4/rec-def-0.2.1/doc/html/rec-def/ docs

rm -rf rec-def-0.2.1/dist-newstyle
rm -rf rec-def-0.2.1/.ghc.environment*


cd /home/artifact/program-anal-datafix
cabal build
rm -rf dist-newstyle



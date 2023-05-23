#!/usr/bin/env bash

#
# This prepares the official AEC base image to
#
#  * install GHC
#  * copy the anonymized library over, and extract it
#  * build and run the test suite, so that cabal dependencies are available
#  * create the documentation for this precise (and anonymized) version of the library
#  * copy the README.md and example files over
#
# It uses virt-customize, which I make available using
#   nix shell -p libguestfs-with-appliance guestfs-tools
#
# It also creates the source artifact tarball, and includes the documentation
# created above therein.

if ! [ -e image/disk.qcow ]
then
  rm -rf base-image image
  tar xaf ./base-image.tar.xz
  mv base-image image
fi

virt-customize \
	--install ghc,ghc-doc,cabal-install \
	--copy-in ../rec-def-0.2.1-anonym.tar.gz:/home/artifact \
	--copy-in aec-image-prepare.sh:/home/artifact \
	--copy-in README.md:/home/artifact \
	--copy-in ../trans.hs:/home/artifact \
	--copy-in ../program-anal.hs:/home/artifact \
	--copy-in ../dominators.hs:/home/artifact \
	--copy-in ../minesweeper.hs:/home/artifact \
	--run-command 'sudo -u artifact mkdir /home/artifact/.ghc' \
	--upload bash_history:/home/artifact/.bash_history \
	--upload ghci_history:/home/artifact/.ghc/ghci_history \
	--run-command 'chown artifact: /home/artifact/.bash_history /home/artifact/.ghc/ghci_history' \
	--run-command 'sudo -u artifact bash /home/artifact/aec-image-prepare.sh' \
	-a image/disk.qcow

cp README.md image

# Prepare source tarball

rm -rf rec-def-artifact

mkdir rec-def-artifact

cd rec-def-artifact || exit
tar xzf ../../rec-def-0.2.1-anonym.tar.gz
cd ..
cp ../trans.hs ../program-anal.hs ../dominators.hs ../minesweeper.hs rec-def-artifact
cp README.md aec-image-build.sh aec-image-prepare.sh rec-def-artifact

virt-copy-out -a image/disk.qcow /home/artifact/docs rec-def-artifact

# Create final tarballs
tar czf rec-def-artifact-image.tar.gz image
tar czf rec-def-artifact-src.tar.gz rec-def-artifact/



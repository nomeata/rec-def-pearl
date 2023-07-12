#!/usr/bin/env bash

set -e

# This prepares the official AEC base image to
#
#  * install GHC
#  * copy the ~~anonymized~~ library over, and extract it
#  * build and run the test suite, so that cabal dependencies are available
#  * create the documentation for this precise ~~(and anonymized)~~ version of the library
#  * copy the README.md and example files over
#
# It uses virt-customize, which I make available using
#   nix-shell -p libguestfs-with-appliance guestfs-tools
#
# It also creates the source artifact tarball, and includes the documentation
# created above therein.

rm -rf base-image image
if ! [ -e base-image.tar.xz ]
then
  wget https://soft.vub.ac.be/~qstieven/icfp/base-image.tar.xz
fi
echo "Unpacking base image"
tar xaf ./base-image.tar.xz
mv base-image image

if ! [ -e rec-def-0.2.1.tar.gz ]
then
  wget https://hackage.haskell.org/package/rec-def-0.2.1/rec-def-0.2.1.tar.gz
fi

virt-customize \
	--smp 8 -m 8000 \
	--install ghc,ghc-doc,cabal-install \
	--copy-in rec-def-0.2.1.tar.gz:/home/artifact \
	--copy-in aec-image-prepare.sh:/home/artifact \
	--copy-in README.md:/home/artifact \
	--copy-in aec-code/trans.hs:/home/artifact \
	--copy-in aec-code/program-anal.hs:/home/artifact \
	--copy-in aec-code/dominators.hs:/home/artifact \
	--copy-in aec-code/minesweeper.hs:/home/artifact \
	--copy-in aec-code/Hatafun.hs:/home/artifact \
	--copy-in aec-code/program-anal-hatafun.hs:/home/artifact \
	--copy-in aec-code/program-anal-datafix:/home/artifact \
	--run-command 'sudo -u artifact mkdir /home/artifact/.ghc' \
	--upload bash_history:/home/artifact/.bash_history \
	--upload ghci_history:/home/artifact/.ghc/ghci_history \
	--run-command 'chown -R artifact: /home/artifact' \
	--run-command 'sudo -u artifact bash /home/artifact/aec-image-prepare.sh' \
	-a image/disk.qcow

echo "Sparsifying"
du -sh image/disk.qcow
virt-sparsify --inplace image/disk.qcow
du -sh image/disk.qcow

cp README.md image

echo "Preparing source tarball"

rm -rf rec-def-artifact

mkdir rec-def-artifact

cd rec-def-artifact || exit
tar xzf ../rec-def-0.2.1.tar.gz
cd ..
cp -r aec-code/* rec-def-artifact
cp README.md aec-image-build.sh aec-image-prepare.sh rec-def-artifact

virt-copy-out -a image/disk.qcow /home/artifact/docs rec-def-artifact

echo "Artifact source created"

echo "Tarring image"
tar czf rec-def-artifact-image.tar.gz image
echo "Tarring source"
tar czf rec-def-artifact-src.tar.gz rec-def-artifact/

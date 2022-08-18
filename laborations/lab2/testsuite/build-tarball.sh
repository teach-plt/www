#!/bin/sh

NAME=lab2-testsuite

mkdir "$NAME"
mkdir "$NAME/good"
mkdir "$NAME/good/subtyping"
mkdir "$NAME/bad"
mkdir "$NAME/bad-runtime"

cp -p plt-test-lab2.hs "$NAME"
cp -p plt-test-lab2.cabal "$NAME"
cp -p prelude.cc "$NAME"
cp Makefile-test "$NAME/Makefile"

cp -p good/*.cc "$NAME/good/"
cp -p good/*.cc.input "$NAME/good/"
cp -p good/*.cc.output "$NAME/good/"
cp -p good/subtyping/*.cc "$NAME/good/subtyping/"
#cp -p good/subtyping/*.cc.input "$NAME/good/subtyping/" <-- currently none
cp -p good/subtyping/*.cc.output "$NAME/good/subtyping/"
cp -p bad/*.cc "$NAME/bad/"
cp -p bad-runtime/*.cc "$NAME/bad-runtime/"

TAR=tar
GTAR=gtar
if ! command -v $TAR >/dev/null; then echo "Command not found: $TAR... Switching to $GTAR."; TAR=$GTAR; fi
$TAR -zcf "$NAME.tar.gz" "$NAME"

rm -rf "$NAME"

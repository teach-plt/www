#!/bin/sh

NAME=lab4-testsuite

mkdir "$NAME"
mkdir "$NAME/bad"
mkdir "$NAME/good"

cp Makefile-test "$NAME"/Makefile
cp plt-test-lab4.hs "$NAME"/
cp plt-test-lab4.cabal "$NAME"/
cp -p bad/*.hs "$NAME/bad/"
cp -p good/*.hs "$NAME/good/"

TAR=tar
GTAR=gtar
if ! command -v $TAR >/dev/null; then echo "Command not found: $TAR... Switching to $GTAR."; TAR=$GTAR; fi
$TAR -zcf "$NAME.tar.gz" "$NAME"

rm -rf "$NAME"

#!/bin/sh

DIR=lab1-testsuite

mkdir "$DIR"
mkdir "$DIR/good"
mkdir "$DIR/bad"

cp progs-test-lab1.hs "$DIR"
cp Makefile-test "$DIR/Makefile"
cp -p good/*.cc "$DIR/good/"
cp -p bad/*.cc "$DIR/bad/"

TAR=tar
GTAR=gtar
if ! command -v $TAR >/dev/null; then echo "Command not found: $TAR... Switching to $GTAR."; TAR=$GTAR; fi
$TAR -czf "$DIR.tar.gz" "$DIR"

rm -rf "$DIR"

#!/bin/sh

NAME=lab1-testsuite

mkdir "$NAME"
mkdir "$NAME/good"
mkdir "$NAME/bad"

cp plt-test-lab1.hs "$NAME"
cp Makefile-test "$NAME/Makefile"
cp -p good/*.cc "$NAME/good/"
cp -p bad/*.cc "$NAME/bad/"

tar -zcf "$NAME.tar.gz" "$NAME"

rm -rf "$NAME"

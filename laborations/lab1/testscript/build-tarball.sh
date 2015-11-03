#!/bin/sh

NAME=lab1-testsuite

mkdir "$NAME"
mkdir "$NAME/good"
mkdir "$NAME/bad"

cp progs-test-lab1.hs "$NAME"
cp Makefile-test "$NAME/Makefile"
cp ../test/*.cc "$NAME/good/"
cp ../test/bad/*.cc "$NAME/bad/"

tar -zcf "$NAME.tar.gz" "$NAME"

rm -rf "$NAME"

#!/bin/sh

NAME=lab4-testsuite

mkdir "$NAME"
mkdir "$NAME/bad"
mkdir "$NAME/good"

cp Makefile-test "$NAME"/Makefile
cp progs-test-lab4.hs "$NAME"/
cp -p bad/*.hs "$NAME/bad/"
cp -p good/*.hs "$NAME/good/"

tar -zcf "$NAME.tar.gz" "$NAME"

rm -rf "$NAME"

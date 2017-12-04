#!/bin/sh

NAME=lab4-testsuite

mkdir "$NAME"
mkdir "$NAME/bad"
mkdir "$NAME/good"

cp progs-test-lab4 "$NAME"
cp -p bad/*.hs "$NAME/bad/"
cp -p good/*.hs "$NAME/good/"

tar -zcf "$NAME.tar.gz" "$NAME"

rm -rf "$NAME"

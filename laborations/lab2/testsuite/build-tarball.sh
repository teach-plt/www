#!/bin/sh

NAME=lab2-testsuite

mkdir "$NAME"
mkdir "$NAME/good"
mkdir "$NAME/bad"

cp -p progs-test-lab2.hs "$NAME"
cp -p good/*.cc "$NAME/good"
cp -p good/*.cc.input "$NAME/good"
cp -p good/*.cc.output "$NAME/good"
cp -p bad/*.cc "$NAME/bad"

tar -zcf "$NAME.tar.gz" "$NAME"

rm -rf "$NAME"
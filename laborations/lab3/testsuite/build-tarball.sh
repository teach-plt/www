#!/bin/sh

NAME=lab3-testsuite

mkdir "$NAME"
mkdir "$NAME/good"
# mkdir "$NAME/bad"

cp -p progs-test-lab3.hs "$NAME"
cp -p jasmin.jar         "$NAME"
cp -p Runtime.java       "$NAME"
cp -p Makefile           "$NAME"
cp -p index.txt          "$NAME"
cp -p good/*.cc        "$NAME/good"
cp -p good/*.cc.input  "$NAME/good"
cp -p good/*.cc.output "$NAME/good"
# cp -p bad/*.cc "$NAME/bad"

tar -zcf "$NAME.tar.gz" "$NAME"

rm -rf "$NAME"

# EOF

#!/bin/sh

NAME=lab3-testsuite

mkdir "$NAME"
mkdir "$NAME/good"
mkdir -p "$NAME/dir-for-path-test/one-more-dir"
mkdir "$NAME/bin"

cp -p progs-test-lab3.hs "$NAME"
cp -p Runtime.java       "$NAME"
cp -p Makefile-test      "$NAME/Makefile"
cp -p index.txt          "$NAME"
cp -p good/*.cc        "$NAME/good/"
cp -p good/*.cc.input  "$NAME/good/"
cp -p good/*.cc.output "$NAME/good/"
cp -p dir-for-path-test/one-more-dir/simple.cc "$NAME/dir-for-path-test/one-more-dir/"
cp -p bin/jasmin        "$NAME/bin/"
cp -p bin/jasmin.jar    "$NAME/bin/"

tar -zcf "$NAME.tar.gz" "$NAME"

rm -rf "$NAME"

# EOF

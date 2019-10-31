#!/bin/sh

DIR=lab1-testsuite

mkdir "$DIR"
mkdir "$DIR/good"
mkdir "$DIR/bad"

cp progs-test-lab1.hs "$DIR"
cp Makefile-test "$DIR/Makefile"
cp -p good/*.cc "$DIR/good/"
cp -p bad/*.cc "$DIR/bad/"

tar -czf "$DIR.tar.gz" "$DIR"

rm -rf "$DIR"

#!/bin/sh

NAME=lab4-testsuite

mkdir "$NAME"

cp progs-test-lab4.sh "$NAME"
cp -p *.hs "$NAME"

tar -zcf "$NAME.tar.gz" "$NAME"

rm -rf "$NAME"

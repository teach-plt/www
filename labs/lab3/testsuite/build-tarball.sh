#!/usr/bin/env sh

NAME=lab3-testsuite

mkdir "$NAME"
mkdir "$NAME/good"
mkdir "$NAME/good/subtyping"
mkdir -p "$NAME/dir-for-path-test/one-more-dir"

cp -p plt-test-lab3.hs "$NAME"
cp -p plt-test-lab3.cabal "$NAME"
cp -p prelude.cc         "$NAME"
cp -p jasmin.jar         "$NAME"
cp -p Runtime.java       "$NAME"
cp -p Makefile-test      "$NAME/Makefile"
cp -p good/*.cc        "$NAME/good/"
cp -p good/*.cc.input  "$NAME/good/"
cp -p good/*.cc.output "$NAME/good/"
cp -p good/subtyping/*.cc        "$NAME/good/subtyping/"
#cp -p good/subtyping/*.cc.input  "$NAME/good/subtyping/" <-- currently none
cp -p good/subtyping/*.cc.output "$NAME/good/subtyping/"
cp -p dir-for-path-test/one-more-dir/simple.cc "$NAME/dir-for-path-test/one-more-dir/"

TAR=tar
GTAR=gtar
if ! command -v $TAR >/dev/null; then echo "Command not found: $TAR... Switching to $GTAR."; TAR=$GTAR; fi
$TAR -zcf "$NAME.tar.gz" "$NAME"

rm -rf "$NAME"

# EOF

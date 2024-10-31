#!/usr/bin/env sh

if [ "$1" == "" -o  "$1" == "-h" -o "$1" == "--help" ]; then
  echo "PLT lab 1 testsuite runner"
  echo "usage: $0 GRAMMARFILE"
  exit 1
fi

cabal run plt-test-lab1 -- "$1"

# EOF

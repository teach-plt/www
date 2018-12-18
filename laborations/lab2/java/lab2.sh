#!/bin/sh

# A simple shell program to run PLT lab2 on Linux/Unix systems.

# Adds the current dir to the class path so that lab2.class
# is found even when called from another directory.

# Rename this to  lab2  to use with the testsuite.

dir=`dirname $0`
exec java -cp "$dir:$CLASSPATH" lab2 "$@"

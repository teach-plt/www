#!/bin/sh

# A simple shell program to run PLT lab4 on Linux/Unix systems.

# Adds the current dir to the class path so that lab4.class
# is found even when called from another directory.

# Rename this to  lab4  to use with the testsuite.

dir=`dirname $0`
exec java -Xss80m -cp "$dir:$CLASSPATH" lab4 "$@"

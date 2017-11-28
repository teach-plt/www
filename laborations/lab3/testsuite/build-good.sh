#!/bin/bash

lab=../../lab2/testsuite/good
files=`grep  -L -r -i --include \*.cc "double" $lab`

if [ -d good ]; then
  echo "Directory 'good' exists."
  echo "Remove it if you want to rebuild it from $lab."
  echo "This will pick all the testcases that do not mention 'double'."
  exit 1
fi

mkdir good

for file in $files; do
  cp -p $file good/
  cp -p $file.input good/ 2> /dev/null
  cp -p $file.output good/
done

echo "Rebuilt 'good' from $lab."

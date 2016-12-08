#!/bin/bash
# Test suite for Lab 4

if [ "$#" -ne 1 ]; then
    echo "usage: ${0} PATH_TO_SOLUTION"
    exit
fi

testdir=`pwd`
codedir="${1}"

set -o errexit
cd $codedir
make
set +o errexit

# Good tests
arr[0]="good1.hs -v 7"
arr[1]="good2.hs -n 5"
arr[2]="good3.hs -v 5050"
arr[3]="good4.hs -v 720"
arr[4]="good5.hs -n 0"
arr[5]="good6.hs -v 1073741824"
arr[6]="good7.hs -v 1"
arr[7]="good8.hs -v 8"
arr[8]="good9.hs -v 131072"
arr[9]="good10.hs -v 1"
arr[10]="good10.hs -n 1"
arr[11]="good11.hs -v 1"
arr[12]="good11.hs -n 1"

for index in "${!arr[@]}"
do
    s=${arr[$index]}
    a=($s)
    file=${a[0]}
    mode=${a[1]}
    expect=${a[2]}
    echo -e "\033[34m""--- $file ---""\033[0m"
    echo "     Mode: ${mode}"
    echo "Expecting: ${expect}"
    result=`./lab4 ${mode} ${testdir}/${file}`
    exitval=$?

    if [ $exitval -ne 0 ]; then
        echo -e "\033[31m""Error""\033[0m"
    else
        if [ "${result}" -eq "${expect}" ]; then
            echo -e "   Output: \033[32m"$result"\033[0m"
        else
            echo -e "   Output: \033[31m"$result"\033[0m"
        fi
    fi

    echo
done

# Bad tests
for F in `ls "${testdir}"/bad*.hs`
do
    echo -e "\033[34m""xxx" `basename ${F}` "xxx""\033[0m"
    ./lab4 "$F"
    echo
done

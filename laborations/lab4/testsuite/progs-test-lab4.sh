#!/bin/bash
# Test suite for lab 4

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 path_to_solution"
    exit
fi

testdir=$(pwd)
codedir=$1

set -o errexit
cd "$codedir"
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
arr[13]="good12.hs -v 0"
arr[14]="good13.hs -v 1"
arr[15]="good14.hs -n 33"

goodtot=0
goodpass=0
for good in "${arr[@]}"; do
    ((goodtot+=1))
    a=($good)
    file=${a[0]}
    mode=${a[1]}
    expect=${a[2]}
    echo -e "\033[34m--- $file ---\033[0m"
    echo "     Mode: $mode"
    echo "Expecting: $expect"
    result=$(./lab4 "$mode" "$testdir"/"$file")
    exitval=$?

    if [ $exitval -ne 0 ]; then
        echo -e "\033[31mError\033[0m"
    else
        if [ "$result" -eq "$expect" ]; then
	    ((goodpass+=1))
            echo -e "   Output: \033[32m$result\033[0m"
        else
            echo -e "   Output: \033[31m$result\033[0m"
        fi
    fi

    echo
done

# Bad tests
badtot=0
badpass=0
for bad in "${testdir}"/bad*.hs; do
    ((badtot+=1))
    echo -e "\033[34mxxx" "$(basename "$bad")" "xxx\033[0m"
    result=$(./lab4 "$bad" 2>&1)
    echo "$result"
    if echo "$result" | grep -Ei "^(INTERPRETER ERROR|java.lang.RuntimeException|ERROR)" > /dev/null; then
	((badpass+=1))
    fi
    echo
done

# Summary
echo "### Summary ###"
echo "$goodpass of $goodtot good tests passed."
echo "$badpass of $badtot bad tests passed (approximate check, only checks if any error at all was reported)."

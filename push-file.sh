#!/bin/sh

# Push a single file to server

# Check args
if [ $# -lt 1 ]; then
    echo "Usage: ./push-file.sh FILE1 [FILE2 [...]]"
    exit
fi

echo "Pushing individual file[s]"

read -p "Enter CID: " username

for file in "$@"
do
    # Check file exists
    if [ ! -f "$file" ]; then
        echo "File \"${file}\" not found"
    else
        # Copy it
        scp ${file} ${username}@remote12.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/course/DAT151-lp2/${file}
    fi
done

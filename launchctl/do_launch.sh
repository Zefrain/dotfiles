#!/bin/bash


files=$(realpath *.plist)

for file in $files
do
    echo "start $file..."
    launchctl unload $file
    launchctl load $file
    echo "done... "
done

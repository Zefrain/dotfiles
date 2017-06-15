#!/bin/bash


remote="zhoushang@192.168.254.58"

for file in $@
do
    src=$(realpath $file)
    dst=$(echo "${remote}:~/$(dirname ${src/\/Users\/zhoush\/Working\/zxpay/})")
    rsync -avz --exclude-from="/Users/zhoush/Git/sync/exclude.list" $src $dst
done

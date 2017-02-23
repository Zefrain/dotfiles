#!/bin/bash


err() {
    echo "[$(date +'%Y-%m-%d %H:%M:%S')]: $@" >&2
}
fmt() {
    OLD=$IFS
    IFS=$'\n'
    for fname in $@; do
        clang-format -i -style=google $fname
    done;
    IFS=$OLD
}
flist=$(find ! -name '.*' | egrep -v '\.\w+\/' | egrep '\.(c|cpp|h)$')

fmt $flist

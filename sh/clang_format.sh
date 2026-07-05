#!/bin/bash

err() {
  echo "[$(date +'%Y-%m-%d %H:%M:%S')]: $*" >&2
}
fmt() {
  for fname in "$@"; do
    clang-format -i -style=google "$fname"
  done
}

mapfile -t flist < <(find . -type f ! -path '*/.*/*' \( -name '*.c' -o -name '*.cpp' -o -name '*.h' \))
if ((${#flist[@]} > 0)); then
  fmt "${flist[@]}"
else
  err "no C/C++ files found"
fi

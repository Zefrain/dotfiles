#!/bin/bash

flist=$(ag -g '\.(rar|7z)$')
dlist=$(find . -type d -name '*\(1\)')

OLD=$IFS
IFS=$'\n'

for fname in $flist; do
  oldname=$fname
  echo "oldname=$oldname"
  fname=${fname/\(1\)/}
  mv "$oldname" "$fname"
  echo "fname=$fname"

  dname=${fname%\.*}
  echo "dname=$dname"
  outdir="$(dirname "$fname")/$dname"
  echo "outdir=$outdir"

  rm -rf "$outdir" && mkdir "$outdir"
  7z -aoa x "$fname" -o"$outdir" && rm "$fname"

  if [ $? -ne 0 ]; then
    rm -rf "$outdir"

    if [ -d "${outdir}\(1\)" ]; then
      rm "$fname"
      mv "${outdir}\(1\)" "$outdir"
    fi
  fi
done

IFS=$OLD

#!/usr/bin/bash


flist=$(find . -iname '*.rar' -o -iname '*.zip');

OLD=$IFS
IFS=$'\n'

for fname in $flist
do
    oldname=$fname
    echo "oldname=$oldname"
    fname=$(ls $oldname| perl -p -e "s|\.\d\.|.|g")
    mv $oldname $fname
    echo "fname=$fname"

    dname=${fname#*\ }
    dname=${dname%\.*}
    echo "dname=$dname"
    outdir="$(dirname "$file")/$dname"
    echo "outdir=$outdir"
    rm -rf $outdir && mkdir $outdir
    7z -aoa x $fname -o$outdir && rm $fname
done

IFS=$OLD

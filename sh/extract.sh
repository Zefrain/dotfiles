#!/bin/bash


# flist=$(find . -iname '*.rar' -o -iname '*.7z');
flist=$(ag -g '\.(rar|7z)$')
# dlist=$(ag -g '\(1\)\.(rar|7z)$')
dlist=$(find . -type d -name '*\(1\)')

OLD=$IFS
IFS=$'\n'

for fname in $flist
do
    oldname=$fname
    echo "oldname=$oldname"
    # fname=$(ls $oldname| perl -p -e "s|\.\d\.|.|g")
    fname=${fname/\(1\)}
    mv $oldname $fname
    echo "fname=$fname"

    dname=${fname%\.*}
    echo "dname=$dname"
    outdir="$(dirname "$file")/$dname"
    echo "outdir=$outdir"

    rm -rf $outdir && mkdir $outdir
    7z -aoa x $fname -o$outdir && rm $fname

    if [ $? != 0 ] ; then
        rm -rf "$outdir"

        if [ -d "${outdir}\(1\)" ] ; then
            rm $fname; mv "${outdir}\(1\)" "$outdir"
        fi
    fi
done

# for dname in $dlist
# do
#     dst=${dname/\(1\)/}
#     rm -rf "$dst"
#     echo $dname
#     echo $dst
#     mv "$dname" "$dst"
# done


IFS=$OLD

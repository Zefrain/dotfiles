#!/bin/sh
set -e
#var

from="/home/zhoushang/Documents/BambooCloudBDC/"
to="root@192.168.4.210:~/BambooCloudBDC"


#function
inotify_fun ()
{
    f=$1
    /usr/bin/inotifywait -mrq --timefmt '%Y%m%d-%H:%M' --format '%T %e %w%f' \
                         -e CLOSE_WRITE,delete,create,move $f|while read time file
    do
        for t in $to
        do
            cmd="rsync -avzuq --delete --progress --prune-empty-dirs --files-from=$f/cscope.files $f $t"
            echo `date +%Y%m%d-%T`: $cmd
            eval $cmd
        done
    done
}
#main

main()
{
    for a in $from
    do
        inotify_fun $a
    done
}

main

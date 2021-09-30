#!/bin/sh
set -e
#var

from=""
to=""


#function
inotify_fun ()
{
    /usr/bin/inotifywait -mrq --timefmt '%Y%m%d-%H:%M' --format '%T %e %w%f' \
                         -e CLOSE_WRITE,delete,create,move $1|while read time file
    do
        f=$1
        for t in $to
        do
            cmd="rsync -avzq --delete --progress --prune-empty-dirs --include=\"*.c\" --include \"*.h\" --include=\"**HELP/***\" --exclude=\"*\" $f $t"
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

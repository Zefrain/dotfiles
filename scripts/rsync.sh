#!/bin/sh
src=/var/abs
des=/home/zhoush/Downloads
/usr/bin/inotifywait -mrq --timefmt '%Y%m%d-%H:%M' --format  '%T %e %w%f' \
 -e CLOSE_WRITE,delete,create,attrib \
${src} \
| while read  file
        do
                rsync -avz --delete --progress ${src} ${des} &&
                echo "${src} was rsynced"
                echo "---------------------------------------------------------------------------"
        done

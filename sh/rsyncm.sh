#!/bin/sh
#set -x
#var 
src="/home/zhoush/Projects/radiusbin/wlanzb /home/zhoush/Projects/telecomvpdn/cproject/vpdn_data_process/src/intervpdn_dodb  /home/zhoush/Projects/radiusbin/radiuscap/interaaa_dodb"

des_ip="172.16.31.156 172.16.31.219"


#function
inotify_fun ()
{
/usr/bin/inotifywait -mrq --timefmt '%Y%m%d-%H:%M' --format '%T %e %w%f' \
-e CLOSE_WRITE,delete,create,move $1|while read time file
do
for ip in $des_ip
do
echo "`date +%Y%m%d-%T`: rsync -avzq --delete --progress $1 $ip:/home/zhoushang/Projects"
rsync -avzq --delete --progress $1 $ip:/home/zhoushang/Projects
echo
done
done
}
#main
for a in $src
do
inotify_fun $a & 
 
done

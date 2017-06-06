#!/bin/bash
stop() {
    # must interrupt it to save session
    killall -2 aria2c
    echo 'stop'
}
start(){
    usr_path=$HOME					#基础目录
    aria2_conf_path="$usr_path/.aria2"
    #配置文件目录
    aria2_downloadfolder="/home/dream/download/aria2"
    #下载文件目录

    aria2_configfile="$aria2_conf_path/aria2.conf"		#配置
    aria2_downloadlist="$aria2_conf_path/aria2file.txt"
    #下载任务列表
    aria2_DHT="$aria2_conf_path/dht.dat"
    #dht数据文件
    [ ! -d "$aria2_conf_path" ] && mkdir -p "$aria2_conf_path"
    [ ! -f "$aria2_downloadlist" ] && touch "$aria2_downloadlist"
    [ ! -f "$aria2_DHT" ] && touch "$aria2_DHT"
    [ ! -f "$aria2_configfile" ] && {
        cat > "$aria2_configfile" << EOF
# General Setting
#
continue=true
peer-id-prefix=-TR2610-
user-agent=Transmission/2.61 (13407)
event-poll=epoll
#on-download-complete=/etc/aria2/post

# Connection Setting
#
disable-ipv6
check-certificate=false
min-split-size=5M

# BitTorrent Setting
#
enable-dht
enable-dht6=false
enable-peer-exchange
bt-enable-lpd
bt-seed-unverified
bt-save-metadata
bt-hash-check-seed
bt-remove-unselected-file
bt-stop-timeout=900
seed-ratio=0.0
save-session-interval=60
EOF
    }
    seedtime=30           #做种时间
    diskcache="100M"			#磁盘缓存
    fileallocation="none"	#磁盘预分配
    download_limit="600K"	#总体下载速度限制
    upload_limit="50K"		#总体上传速度限制
    btmaxpeers="20"       #每个种子最大peer数量,0为不限制
                          # maxthread="10" #下载使用连接数
    maxjobs="4"           #同时开启任务数量

    # cmd="aria2c -c -D --enable-rpc --rpc-listen-all=true --rpc-allow-origin-all --seed-time=$seedtime --conf-path=$aria2_configfile --input-file=$aria2_downloadlist --save-session=$aria2_downloadlist --disk-cache=$diskcache --dht-file-path=$aria2_DHT --file-allocation=$fileallocation --max-overall-download-limit=$download_limit --max-overall-upload-limit=$upload_limit --bt-max-peers=$btmaxpeers --split=$maxthread --max-connection-per-server=$maxthread --max-concurrent-downloads=$maxjobs --listen-port=$tcp_port --dht-listen-port=$udp_port --force-save=true"
    cmd="aria2c -c -D --conf-path=$aria2_configfile"
    [ "`pgrep aria2c`" != "" ] && {
        stop
        sleep 3
    }
    echo 'starting...'
    eval "$cmd"
}
restart(){
    stop
    sleep 3
    start
}
action=${1:-"start"}
case "$action" in
    start)
        start
        ;;
    stop)
        stop
        ;;
    reload|force-reload|restart)
        restart
        ;;
    status|pid)
        pgrep aria2c
        ;;
    *)
        echo $"Usage: $0 {start|stop|restart|pid}"
        exit 1
esac

if [ ! -z $1 ]; then
    pgrep $1 | xargs -n 1 kill -9 ||
    ps -ef | grep $1 | awk '{system("kill -9 " $2)}'
else
    echo "please give argument to kill";
fi

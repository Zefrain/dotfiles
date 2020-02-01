if [ ! -z $# ]; then
    pkill $* \
        || pgrep $* | xargs -n 1 kill -9 \
        || ps -ef | grep $* | grep -v "grep" | awk '{system("kill -9 " $2)}' \
        || lsof -nPi | grep $* | awk '{print "kill -9", $2}' | sh
else
    echo "please give argument to kill";
fi


if [ $? -eq 0 ]; then
    echo "success"
else
    echo "failed"
fi

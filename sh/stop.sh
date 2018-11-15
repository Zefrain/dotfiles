if [ ! -z $1 ]; then
    pgrep $1 | xargs -n 1 kill -9 || ps -ef | grep $1 | grep -v "grep" | awk '{system("kill -9 " $2)}' || lsof -nPi | grep $1 | awk '{print "kill -9", $2}' | sh
else
    echo "please give argument to kill";
fi

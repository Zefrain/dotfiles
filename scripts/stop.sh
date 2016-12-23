if [ ! -z $1 ]; then
    pgrep $1 | xargs -n 1 kill -9
else
    echo "please give argument to kill";
fi

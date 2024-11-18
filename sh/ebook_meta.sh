#!/bin/bash

# Author : bookfere.com
# Site   : https://bookfere.com/post/550.html 

# 定义变量
IDEN="书伴 - 为静心阅读而生（bookfere.com）"
LINE="===================================="
SPLI="------------------------------------"
if ! type -t ebook-meta > /dev/null; then
    echo "脚本不可用！请先安装 Calibre 软件：https://bookfere.com/tools#calibre"
    exit
fi
printf "\n$IDEN\n"
if [ ! $1 ]
then
    printf '\n输入电子书文件所在的绝对路径：'
    echo -n
    read DIR
else
    DIR="$1"
fi
if [[ ! -d "$DIR" ]]
then
    printf "\n请输入有效的路径！\n"
    bash $0
fi
LOG="$DIR/eBookMeta.txt"
# 开始处理
echo $LINE > $LOG
cd "$DIR"
echo "目录 $DIR 下的电子书元数据列表" > $LOG
printf "\n开始读取电子书元数据……\n"
echo "$LINE" >> $LOG
OIFS="$IFS"
IFS=$'\n'
for SUB in $(find "$DIR" -maxdepth 5 -type d)
do
    if [[ -d "$SUB" ]]
    then
        cd "$SUB"
        for f in $SUB/*.azw3 $SUB/*.mobi $SUB/*.azw $SUB/*.epub
        do
            if [[ -f "$f" ]]
            then
                echo "$f" >> $LOG
                echo "$SPLI" >> $LOG
                ebook-meta "$f" >> $LOG
                echo $LINE >> $LOG
                printf "\n- 已处理 $f"
            fi
        done
    fi
done
echo $IDEN >> $LOG
printf "\n\n√ 全部处理完毕！\n\n"
# 处理结束
read -n 1 -s -r -p "请按任意键结束此次处理并打开结果。"
open $LOG

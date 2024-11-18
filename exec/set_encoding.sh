#!/bin/bash
#将文件编码更改为UTF-8
#用法
#1. 将文件命名为set_encoding.sh
#2. chmod +x set_encoding.sh
#3. ./set_encoding.sh
#4. 输入目录名称
#5. 输入是否递归更改
#$1表示是否要递归修改文件编码


function change_file_encoing(){
    OLD=$IFS
    IFS=$'\n'
    for file in $(find . -depth 1 -type f -exec grep -Il "" {} \;)
    do
        if [[ -d "$file" && $1 = y && "$file" != "." && "$file" != ".." ]]; then
            cd "$file"
            echo "$file"
            change_file_encoing "$1"
            cd ..
        elif [[ -f "$file" ]];then
            echo "$file"
            dos2unix "$file"
            enca -L zh_CN -x UTF-8 "$file"
        fi;
    done;
    IFS=$OLD
}

read -p "please enter the dir path:" path # 读取目录路径
if [ ! -x "$path" ];    # 判断目录是否存在且是否具有执行权限
then
    echo "dir path not exists"
else
    read -p "please enter if you want to recursive? y/n: " recur  #是否递归
fi

if [ $recur = "y" ];
then
    cd "$path"
    if [ $? = 0 ];
    then
        change_file_encoing "y"     #递归修改文件编码
    fi
else
    cd "$path"
    if [ $? = 0 ];
    then
        change_file_encoing "n"     #非递归修改
    fi
fi

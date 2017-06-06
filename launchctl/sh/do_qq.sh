#!/bin/bash

pgrep QQ

if [ $? -eq 0 ]
then
    killall QQ
else
    open /Applications/QQ.app
fi

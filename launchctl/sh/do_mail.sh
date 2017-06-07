#!/bin/bash

pgrep Mail

if [ $? -eq 0 ]
then
    killall Mail
else
    open /Applications/Mail.app/
fi

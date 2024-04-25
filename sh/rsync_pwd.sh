#!/bin/bash 

set -e

rsync -avzu . zhou@192.168.131.37:~/Documents/$(basename $(realpath .))

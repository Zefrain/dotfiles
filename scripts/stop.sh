#!/usr/bin/sh

pgrep $@ | xargs -n 1 kill -9 

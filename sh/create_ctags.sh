#!/bin/bash

ctags -R  --exclude=.ccls-cache --languages=$1

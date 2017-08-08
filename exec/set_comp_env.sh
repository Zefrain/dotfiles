#!/bin/bash
# Program:
#   This program is to set coding environment
# History
#   release v0.0.1:     2017-02-14     whiothes

C_INCLUDE_PATH="${C_INCLUDE_PATH}:${PWD}:${PWD}/include:${PWD}/../include"
export C_INCLUDE_PATH

LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${PWD}:${PWD}/lib:${PWD}/../lib"
export LD_LIBRAYRY_PATH

LIBRARY_PATH="${LIBRARY_PATH}:${PWD}:${PWD}/lib:${PWD}/../lib"
export LIBRARY_PATH

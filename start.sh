#!/bin/bash

if [[ "$unamestr" == 'Linux' ]]; then
     exename=erl
else
    exename='start //MAX werl.exe'
    #exename='erl.exe'
fi

# PATHS
paths="-pa"
paths=$paths" $PWD/ebin"

start_opts="$paths"

$exename $start_opts -s erlps

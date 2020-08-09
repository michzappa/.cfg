#!/usr/bin/env bash

# script to toggle the conky display
if [[ $(pidof conky) ]]; then
    pkill conky
else
    conky    
fi

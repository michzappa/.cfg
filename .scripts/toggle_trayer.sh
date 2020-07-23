#!/bin/bash

# script to toggle the systray
if [[ $(pidof trayer) ]]; then
    pkill trayer
else
    trayer --align right
fi

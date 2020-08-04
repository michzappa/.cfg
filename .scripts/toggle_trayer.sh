#!/usr/bin/env bash

# script to toggle the systray
if [[ $(pidof trayer) ]]; then
    pkill trayer
else
    trayer \
        --align right \
        --edge top \
        --width 5 \
        --height 19 \
        --distancefrom right \
        --distance 450 \
        --tint 0x2E3440 \
        --transparent true \
        --alpha 0
fi

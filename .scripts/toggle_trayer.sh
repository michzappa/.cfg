#!/bin/bash

# script to toggle the systray
if [[ $(pidof trayer) ]]; then
    pkill trayer
else
    trayer \
        --align right \
        --edge top \
        --width 20 \
        --height 19 \
        --distancefrom right \
        --distance 350 \
        --tint black \
        --transparent true \
        --alpha 0
fi

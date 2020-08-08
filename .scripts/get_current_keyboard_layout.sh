#!/usr/bin/env bash
LG=$(setxkbmap -query | awk '/layout/{print $2}') 
echo "${LG^^}" || exit 1

#!/usr/bin/env bash

status=$(playerctl status)
if [ "$status" == "Playing" ]; then
	artist=$(playerctl metadata artist)
	song=$(playerctl metadata title)
	echo -n "| $artist - $song"
fi

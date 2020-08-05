#!/usr/bin/env bash

running=$(pidof spotify)
if [[ "$running" ]]; then
	artist=$(playerctl metadata artist)
	song=$(playerctl metadata title)
	echo -n "$artist - $song"
fi

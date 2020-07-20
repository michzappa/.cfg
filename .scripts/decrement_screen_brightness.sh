#!/bin/bash
current_brightness=$("$HOME/.scripts/get_screen_brightness.sh")

new_brightness=$(awk "BEGIN {print $current_brightness-0.1    ; exit}")

xrandr --output eDP-1 --brightness $new_brightness
echo "$current_brightness, $new_brightness" || exit 1


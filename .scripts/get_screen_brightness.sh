#!/bin/bash

xrandr_brightness=$(xrandr --current --verbose | grep "Brightness" | awk '{print $2}')

echo "$xrandr_brightness" || exit 1

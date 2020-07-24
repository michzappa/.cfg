#!/bin/bash
current_backlight=$("$HOME/.scripts/get_screen_backlight.sh")

new_backlight=$(( $current_backlight + 5000))

echo "$new_backlight" | sudo tee /sys/class/backlight/intel_backlight/brightness
echo "$current_backlight, $new_backlight"

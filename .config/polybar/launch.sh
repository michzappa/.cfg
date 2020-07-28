#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch Polybar, using default config location ~/.config/polybar/config
wifi_interface=$("$HOME/.scripts/get_wifi_interface_name.sh")

wifi_interface=$wifi_interface polybar mybar &

echo "Polybar launched..."
echo "$wifi_interface"

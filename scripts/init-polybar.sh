#!/usr/bin/env sh

polybar_conf="/home/ryan/.config/polybar/desktop"

case $HOSTNAME in
    (Southpark) polybar_conf="/home/ryan/.config/polybar/desktop" ;;
    (*) polybar_conf="/home/ryan/.config/polybar/config" ;;
esac

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch Polybar, using default config location ~/.config/polybar/config
polybar --reload main -c "$polybar_conf" &

echo "Polybar launched..."

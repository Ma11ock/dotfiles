#!/usr/bin/env zsh

# Sets up my monitors

xrandr --output HDMI-A-2 --auto --left-of DisplayPort-1 --output DisplayPort-1 \
       --rate 144 --primary --mode 2560x1440

xrandr --output DisplayPort-3 --mode 1920x1080 --same-as HDMI-A-2

sleep 2
     
nitrogen --restore

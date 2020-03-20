#!/usr/bin/env zsh

# Sets up my monitors

xrandr --output HDMI-A-2 --auto --left-of DisplayPort-2 --output DisplayPort-2 \
       --rate 144 --primary --mode 2560x1440

xrandr --output DisplayPort-1 --mode 1920x1080 --same-as HDMI-A-2

sleep 3
     
nitrogen --restore

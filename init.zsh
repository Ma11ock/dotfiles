#!/usr/bin/env zsh

curd="$(pwd)"
h="$HOME"

ln -s $curd/.emacs.d $h/.emacs.d
ln -s $curd/.Xresources $h/.Xresources
ln -s $curd/.config $h/.config
ln -s $curd/.joe $h/.joe


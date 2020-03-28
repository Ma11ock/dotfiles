#!/usr/bin/env zsh

pacman -Syy && pacman -Su --noconfirm


latestKernel=$(mhwd-kernel -l | grep -v rt | tail -1 | sed "s/\s*//g ; s/*//g")
latestInstalled=$(mhwd-kernel -li | grep -v rt | tail -1 | sed "s/\s*//g ; s/*//g")

[[ $latestKernel != $latestInstalled ]] && mhwd-kernel -i "$latestKernel"

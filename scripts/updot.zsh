#!/usr/bin/env zsh

# Takes all config files and places them into ~/src/dotfiles/

# location of all dotfiles
home_dir="/home/ryan/"

# Non .config files
dotfile_locals=(".zshrc" ".emacs" ".econfig.org" ".ncmpcpp" "src/scripts")

# The .config folder
config_dir="$home_dir/.config/"

# Config files in .config
config_locals=("i3" "i3status" "mupen64plus" "m64py" "mpd" "polybar" "ranger")

# The folder to place all dotfiles into
dest_dir="/home/ryan/src/dotfiles/"



# folders to exclude from .config (if globbing)
exclude=("Brave")

cd $dest_dir

# Copy dotfiles
for config in $dotfile_locals; do
    file=$(echo "$home_dir$config")
    [[ ${exclude[(ie)$file]} -le ${#exclude}  ]] || cp "$file" $config -r
done

stat .config &>/dev/null || mkdir .config

cd .config

# Copy config files
for config in $config_locals; do
    file=$(echo "$config_dir$config")
    [[ ${exclude[(ie)$file]} -le ${#exclude}  ]] || cp "$file" "$config" -r
done


#!/usr/bin/env zsh

# Takes all config files and places them into ~/src/dotfiles/

# location of all dotfiles
root_dir="/home/ryan/"

dest_dir="/home/ryan/src/dotfiles/"

dotfile_locals=(".config/"{i3,i3status,mupen64plus,m64py,mpd/mpd.conf,polybar,ranger} ".zshrc" ".emacs" ".econfig.org" ".ncmpcpp")




# folders to exclude from .config
exclude=("Brave")

cd $dest_dir

# Copy all files
for config in $dotfile_locals; do
    file=$(echo "$root_dir$config")
    [[ ${exclude[(ie)$file]} -le ${#exclude}  ]] || cp "$file" ./. -r
done

# Move all non-dotfiles back into .config

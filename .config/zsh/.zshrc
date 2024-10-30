# Programming language globals.
export GEM_HOME="$HOME/.cache/gems"
export SCRIPTS="$HOME/src/dotfiles/scripts/:$HOME/src/scripts"
export GOARCH="amd64"
export GOOS="linux"
export CC="gcc"
export CXX="g++"
export CGO_ENABLED="1"
export GOROOT="/usr/lib/go"
export GOBIN="$HOME/src/go/bin/"
export GOPATH="$HOME/src/go/"
export DOOMDIR="$HOME/.config/doom"
export DOOMBIN="$HOME/.config/emacs/bin"
export PATH="$PATH:$GOROOT/bin:/usr/lib/go/bin/:$SCRIPTS:$HOME/bin/:/home/ryan/.local/share/gem/ruby/3.0.0/bin:$HOME/.pub-cache/bin:$HOME/.local/bin:$DOOMBIN"


# Path to your Oh My Zsh installation.
export ZSH="$ZDOTDIR/ohmyzsh"

# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="bira"

plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# Lang stuff.
export DICPATH='/usr/share/hunspell'
export LANG='en_US.UTF-8'

# Globals that various programs use.export MUSIC="$HOME/Music/"
export TERMINAL='kitty-xterm'
export BROWSER='brave'
export COLORTERM='truecolor'
export NODE_PATH="$HOME/src/node_modules"
# TODO patch mednafen to use .config/ by default
export MEDNAFEN_HOME="$MY_CONF_DIR/mednafen/"


# Editor and zsh
export EDITOR="emacsclient -t -a emacs"
export VISUAL="emacsclient -c -a emacs"
# Shortcuts to config files and folders
alias cfk="$EDITOR $MY_CONF_DIR/kitty/kitty.conf "
alias srz='source ~/.config/zsh/.zshrc'
alias jrc='joe $MY_CONF_DIR/joestar/joestarrc'
if type "nvim" &>/dev/null; then
    alias vim='nvim'
    alias cfv="nvim $MY_CONF_DIR/nvim/init.lua"
else
    alias cfv="vim $HOME/.vimrc"
fi
alias cfz="$EDITOR $MY_CONF_DIR/zsh/.zshrc"
alias cfe="$EDITOR $MY_CONF_DIR/emacs/init.el"
# default options and shortcuts
alias rip='abcde -o opus'
alias ls='ls --hyperlink=auto --color -h --group-directories-first'
alias lmk='latexmk -lualatex -synctex=1 -pvc'

# Arch based vs debian based package manager aliases
if type "pacman" &>/dev/null; then
    alias up='paru -Syu --noconfirm'
    alias pac='sudo pacman -Syu --noconfirm'
    alias aur='paru -Syu --noconfirm'
    alias purge='sudo pacman -R'
elif type "apt" &>/dev/null; then
    alias up='sudo apt-get update && sudo apt-get upgrade'
    alias inst='sudo apt-get install'
    alias purge='sudo apt purge'
fi

alias redocmake='../; rm -rf bin; mkdir bin; cd bin; cmake .. -DCMAKE_BUILD_TYPE=debug'
alias ll='ls -l'
alias untar='tar -xvf'
alias ztar='tar -cvJf'
alias latex='lualatex'
alias cls='clear'
alias mkd='mkdir -pv'
alias mkdir='mkdir -pv'
alias dvdtube="youtube-dl -f 'bestvideo[height<=480,ext=mp4]+bestaudio/best[ext=mp3]/mp4'"
alias youaud='youtube-dl --output "%(title)s.%(ext)s" --extract-audio --audio-format mp3 --audio-quality 0'
alias otheru="youtube-dl -f 'bestvideo[height<=480,ext=mp4]+bestaudio[ext=mp3]/mp4' --write-all-thumbnails --merge-output-format mp4"
alias you7="youtube-dl -f 'bestvideo[height<=720,ext=mp4]+bestaudio[ext=mp3]/mp4' --write-all-thumbnails --merge-output-format mp4"
alias rm='rm -v'
alias comicv='ls -v | sxiv -'
alias ec='emacsclient -c'
alias et='emacsclient -t'
# Basically an alias for man

# emacs aliases

alias magit="emacsclient -t -e  '(progn (magit) (delete-other-windows))'"

# Zinit and plugins outside of oh-my-zsh plugin system.
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
[ ! -d $ZINIT_HOME ] && mkdir -p "$(dirname $ZINIT_HOME)"
[ ! -d $ZINIT_HOME/.git ] && git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
source "${ZINIT_HOME}/zinit.zsh"

zinit ice depth=1
zinit light jeffreytse/zsh-vi-mode

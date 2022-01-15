
# For emacs TRAMP mode.
if [[ $TERM == "tramp" ]]; then
    unsetopt zle
    unsetopt prompt_cr
    PS1='$ '
    return
fi

# Fixes old versions of tmux
if [[ -n "$TMUX" && $(tmux -V | awk '{print $2}' | head -c3) -lt "1.8" ]]
then
        PROMPT_COMMAND="printf '\ePtmux;\e\e[<u\e\\'"
fi

autoload -U colors && colors
autoload -Uz vcs_info
precmd() { vcs_info }

# Key hash, for better keybinds.
typeset -A key

key[Home]=${terminfo[khome]}
key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}


# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
setopt PROMPT_SUBST

ssh_info() {
  [[ "$SSH_CONNECTION" != '' ]] && echo '%(!.%{$fg[red]%}.%{$fg[yellow]%})%n%{$reset_color%}@%{$fg[green]%}%m%{$reset_color%}:' || echo ''
}


setopt autocd
stty stop undef

autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
#_comp_options+=(globdots)		# Include hidden files.

# Ctrl-Arrow keys
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word

bindkey '^[[P' delete-char
bindkey '^[[3~' delete-char 

HISTSIZE=10000
SAVEHIST=10000
# Make sure to create this dir and touch history when installing on new system
HISTFILE="$HOME/.cache/zsh/history"

# Write to history immediately
setopt inc_append_history
# History shared among terminals
setopt share_history
# Save extended info in history
setopt extended_history
# Ignore duplicates
setopt hist_ignoredups

# Directory stack
export DIRSTACKSIZE=9
setopt autopushd pushdminus pushdsilent pushdtohome

# Aliases for directory stack
alias 1='cd -1'
alias 2='cd -2'
alias 3='cd -3'
alias 4='cd -4'
alias 5='cd -5'
alias 6='cd -6'
alias 7='cd -7'
alias 8='cd -8'
alias 9='cd -9'

COMPLETION_WAITING_DOTS="true"

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'


#  en_US.UTF-8 UTF-8
export DICPATH='/usr/share/hunspell'

# Programming language globals.
export LANG='en_US.UTF-8'
export SCRIPTS="$HOME/src/dotfiles/scripts/:$HOME/src/scripts"
export GOARCH="amd64"
export GOOS="linux"
export CC="gcc"
export CXX="g++"
export CGO_ENABLED="1"
export GOROOT="/usr/lib/go"
export GOBIN="$HOME/src/go/bin/"
export GOPATH="$HOME/src/go/"
export PATH="$PATH:$GOROOT/bin:/usr/lib/go/bin/:$SCRIPTS:$HOME/bin/"

# The HURD does not have /sbin in its path
(uname -a | grep "gnu-mach" -qi) && export PATH="$PATH:/sbin"

export MUSIC="$HOME/Music/"
export TERMINAL='kitty-xterm'
export BROWSER='brave'
export COLORTERM='truecolor'
export NODE_PATH="$HOME/src/node_modules"
# TODO patch mednafen to use .config/ by default
export MEDNAFEN_HOME="$HOME/.config/mednafen/"


# Editor and zsh
export EDITOR="emacsclient -t -a emacs"
export VISUAL="emacsclient -c -a emacs"

alias cfk="$EDITOR $HOME/.config/kitty/kitty.conf "
alias magit="emacsclient -t -e  '(progn (magit) (delete-other-windows))'"
alias srz='source ~/.zshrc'
alias jrc='joe $HOME/.config/joestar/joestarrc'
alias vim='nvim'
alias cfz="$EDITOR $HOME/.config/zsh/.zshrc"
alias cfe="$EDITOR $HOME/.config/emacs/config.org $HOME/.config/emacs/init.el"
alias jrd='joe -rdonly'
# default options and shortcuts
alias rip='abcde -o opus'
alias ls='ls --hyperlink=auto --color -h --group-directories-first'
alias lmk='latexmk -lualatex -synctex=1 -pvc'

# Arch based vs debian based package manager aliases
if type "pacman" &>/dev/null; then
    alias up='yay -Syu'
    alias pac='doas pacman -Syu'
    alias aur='yay -Syu'
    alias purge='doas pacman -R'
elif type "apt" &>/dev/null; then
    alias up='doas apt-get update && doas apt-get upgrade'
    alias inst='doas apt-get install'
    alias purge='doas apt purge'
fi

alias dosu='doas -u root -s'
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
# Shortcuts to config files and folders
alias i3cfg='joe ~/.config/i3/config'
alias jrz='joe ~/.zshrc'
alias comicv='ls -v | sxiv -'
alias ec='emacsclient -c'
alias et='emacsclient -t'
# Basically an alias for man
function man() {
    local command="$2($1)"
    [ -z $2 ] && command="$1"
    emacsclient -t -e '(man-mode-shell "'"$command"'")' || man "$@"
}
# Create a list of packages to install to move installations.
function create_pkg_list() {
    pacman -Qqen > pac.lst
    pacman -Qqm > aur.lst
}

# Emacs vterm 
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

setopt interactivecomments

# Set up plugins and extensions.

# Zinit.
source /usr/share/zinit/zinit.zsh 2>/dev/null
# If source zinit is under compload.
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

zinit load "zdharma-continuum/fast-syntax-highlighting"
# Must load after fast-syntax-highlighting
zinit load "zsh-users/zsh-history-substring-search"

# Vim mode. 
# The plugin will auto execute this zvm_after_lazy_keybindings function
# This makes substring history search play nice with vim mode.
function zvm_after_lazy_keybindings() {
    # TODO replace key sequence with "${key[Up]}"
    zvm_bindkey vicmd '^[[A' history-substring-search-up
    zvm_bindkey vicmd '^[[B' history-substring-search-down
    zvm_bindkey vicmd 'k' history-substring-search-up
    zvm_bindkey vicmd 'j' history-substring-search-down
}
# Zsh vi mode.
zinit load "jeffreytse/zsh-vi-mode"
# Also needed for substring history search
zvm_define_widget history-substring-search-up
zvm_define_widget history-substring-search-down
zvm_bindkey viins '^[[A' history-substring-search-up
zvm_bindkey viins '^[[B' history-substring-search-down

# Zsh plugins to use oh-my-zsh themes 
zinit snippet "OMZL::spectrum.zsh"
zinit snippet "OMZL::theme-and-appearance.zsh"
zinit snippet "OMZL::git.zsh"
zinit snippet "OMZP::mercurial"
# oh-my-zsh theme
zinit snippet "OMZT::/af-magic"


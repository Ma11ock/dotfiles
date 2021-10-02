
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

# Vi mode
bindkey -v

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
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

setopt PROMPT_SUBST

ssh_info() {
  [[ "$SSH_CONNECTION" != '' ]] && echo '%(!.%{$fg[red]%}.%{$fg[yellow]%})%n%{$reset_color%}@%{$fg[green]%}%m%{$reset_color%}:' || echo ''
}

color_codes=(1 2 4 5 6 9 10 12 13 14)


color_code_from_str() {
    local h=$(sum <<< "$1" | cut -f1 -d' ')
    local i=$((h % ${#color_codes}))
    printf ${color_codes[i]}
}


user="%F{$(color_code_from_str "$USER")}%n"
host="%F{$(color_code_from_str "$HOST")}%m"
PROMPT='%(?.%F{243}.%F{red})%U${(l:COLUMNS:: :)?}%u
%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b '

function check_last_exit_code() {
  local LAST_EXIT_CODE=$?
  if [[ $LAST_EXIT_CODE -ne 0 ]]; then
      local EXIT_CODE_PROMPT=' '
      EXIT_CODE_PROMPT+="%{$fg[red]%}-%{$reset_color%}"
      EXIT_CODE_PROMPT+="%{$fg_bold[red]%}$LAST_EXIT_CODE%{$reset_color%}"
      EXIT_CODE_PROMPT+="%{$fg[red]%}-%{$reset_color%}"
      echo "$EXIT_CODE_PROMPT"
  fi
}

# TODO make this better
function git_prompt_string() {
    stat .git &>/dev/null && git status | head -n1 | awk '{print $3}'
}

RPROMPT='$(git_prompt_string)'

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
HISTFILE=~/.cache/zsh/history

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
export SCRIPTS="$HOME/src/dotfiles/scripts/"
export GOARCH="amd64"
export GOOS="linux"
export CC="gcc"
export CXX="g++"
export CGO_ENABLED="1"
export GOROOT="/usr/lib/go"
export GOBIN="$HOME/src/goproj/bin/"
export GOPATH="$HOME/src/goproj/"
export PATH="$PATH:$GOROOT/bin:/usr/lib/go/bin/:$SCRIPTS:$HOME/bin/"

# The HURD does have /sbin in its path
(uname -a | grep "gnu-mach" -qi) && export PATH="$PATH:/sbin"

export MUSIC="$HOME/Music/"
export TERMINAL='xterm-256color'
export BROWSER='brave'
export COLORTERM='truecolor'
export NODE_PATH="$HOME/src/node_modules"
export MEDNAFEN_HOME="$HOME/.config/mednafen/"


# Editor and zsh
export EDITOR="emacsclient -t -a emacs"
export VISUAL="emacsclient -c -a emacs"

alias srz='source ~/.zshrc'
alias jrc='joe $HOME/.config/joestar/joestarrc'
alias vim='nvim'
alias cfz="$EDITOR $HOME/.zshrc"
alias cfe="$EDITOR $HOME/.config/emacs/config.org $HOME/.config/emacs/init.el"
alias jrd='joe -rdonly'
# default options and shortcuts
alias rip='abcde -o opus'
alias ls='ls --color -h --group-directories-first'

# Arch based vs debian based package manager aliases
if type "pacman" &>/dev/null; then
    alias up='paru -Syu'
    alias pac='doas pacman -Syu'
    alias aur='paru -Syu'
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

# Emacs vterm 
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

setopt interactivecomments

setopt HIST_IGNORE_ALL_DUPS
autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^[[A" history-beginning-search-backward-end
bindkey "^[[B" history-beginning-search-forward-end
unset HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND

source /usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh  2>/dev/null

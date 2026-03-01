# ~/.zshrc
[[ -o interactive ]] || return

umask 077
setopt NO_CLOBBER
setopt INTERACTIVE_COMMENTS
setopt EXTENDED_GLOB
setopt PIPE_FAIL
setopt NOTIFY
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_SILENT
unsetopt NO_HUP

# No XON/XOFF stop keys to prevent terminal from hanging
if [[ -t 0 ]]; then
  stty stop undef 2>/dev/null
  stty start undef 2>/dev/null
fi

## XDG Stuff
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"

# Remove duplicates
typeset -U path manpath fpath

export COLORTERM="${COLORTERM:-truecolor}"
export EDITOR='emacs -nw'
export VISUAL='emacs -nw'
export PAGER='less'
export LESS='-FRX'
export LESSHISTFILE='-'
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export NULLCMD=cat
export READNULLCMD=less

# Make GPG pinentry more stabil in tty
export GPG_TTY="$(tty 2>/dev/null)"
export CDPATH=".:~:~/Documents/git"
export NPM_CONFIG_PREFIX="$HOME/.local"

## History
# XDG-state for history
_hist_dir="$XDG_STATE_HOME/zsh"
[[ -d "$_hist_dir" ]] || mkdir -p -- "$_hist_dir" 2>/dev/null
export HISTFILE="$_hist_dir/history"

export HISTSIZE=20000
export SAVEHIST=20000


setopt APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY
setopt EXTENDED_HISTORY
setopt HIST_FIND_NO_DUPS

## Completion
autoload -Uz compinit
# Compdump cache
_compdump="$XDG_CACHE_HOME/zsh/zcompdump"
[[ -d "$XDG_CACHE_HOME/zsh" ]] || mkdir -p -- "$XDG_CACHE_HOME/zsh" 2>/dev/null

# Let compinit test insecure directories
compinit -d "$_compdump"

zmodload -i zsh/complist 2>/dev/null

# Completion style
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/zcompcache"

## Emacs bindings
bindkey -e
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

## Substring history search
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

typeset -gA key
key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"

bindkey '^P' up-line-or-beginning-search
bindkey '^N' down-line-or-beginning-search
[[ -n "${key[Up]}" ]] && bindkey "${key[Up]}" up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey "${key[Down]}" down-line-or-beginning-search

## Prompt
autoload -Uz vcs_info add-zsh-hook
setopt PROMPT_SUBST
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' formats '(%b)'
zstyle ':vcs_info:git:*' actionformats '(%b|%a)'
_precmd_vcs() { vcs_info 2>/dev/null; }
add-zsh-hook precmd _precmd_vcs
autoload -Uz colors && colors
PROMPT='%B%F{green}%n@%m%f:%F{blue}%~%f ${vcs_info_msg_0_}%F{blue}%#%f%b '

# Aliases
alias emacs="emacs -nw"
alias ls="lsd"
alias ll="lsd -la"
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias grep='grep --color=auto'
alias clobber='>|'
alias h='history -20'

alias an='ansible'
alias anp='ansible-playbook'
alias anv='ansible-vault'
alias ani='ansible-inventory'

alias cdd='cd ~/Documents/git/devops-infrastructure'
alias cdg='cd ~/Documents/git/'
alias cdh='cd ~/'

hash -d git=~/Documents/git
hash -d infra=~/Documents/git/devops-infrastructure

mkcd() { mkdir -p -- "$1" && cd -- "$1" || return; }

## Local overrides
[[ -f "$ZDOTDIR/local.zsh" ]] && source "$ZDOTDIR/local.zsh"

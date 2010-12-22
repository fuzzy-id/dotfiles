#!/bin/zsh
# completion
autoload -U compinit
compinit

zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*warnings' format '%BSorry, no matches for: %d%b'

# correction
setopt correctall

# prompt
autoload -U promptinit
promptinit
prompt gentoo

export HISTSIZE=100000
export HISTFILE="$HOME/.history"
export SAVEHIST=10000

export PATH="${HOME}/bin:${PATH}"

if [[ -f ~/.alias ]]; then
    source ~/.alias
fi

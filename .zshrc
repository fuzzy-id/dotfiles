# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _ignored
zstyle :compinstall filename '~/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000
# End of lines configured by zsh-newuser-install

export PROMPT='%F{blue}%n@%m%F{green}:%F{red}%~ %F{white}'
export CLICOLOR='on'

if [[ -f ~/.alias ]]; then
    source ~/.alias
fi

if [[ -f ~/.environment ]]; then
    source ~/.environment
fi

if [[ -f /etc/bash_completion.d/virtualenvwrapper ]]; then
    source /etc/bash_completion.d/virtualenvwrapper
fi
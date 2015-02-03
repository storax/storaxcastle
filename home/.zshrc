HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e

# The following lines were added by compinstall
zstyle :compinstall filename '~/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Develop/Python/
source /usr/local/bin/virtualenvwrapper.sh

source ~/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle command-not-found
antigen bundle common-aliases
antigen bundle dirhistory
antigen bundle gem
antigen bundle git
antigen bundle git-flow
antigen bundle pip
antigen bundle sudo
antigen bundle vagrant

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen theme kphoen

# Tell antigen that you're done.
antigen apply


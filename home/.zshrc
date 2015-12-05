HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e

zstyle :compinstall filename '~/.zshrc'

# folder of all of your autocomplete functions
fpath=(~/.zsh-completions $fpath)

autoload -Uz compinit
compinit
autoload -Uz bashcompinit
bashcompinit
source ~/.bash-completions/*

export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/projects/
# source /usr/local/bin/virtualenvwrapper.sh

# build python with enable shared
export PYTHON_CONFIGURE_OPTS="--enable-unicode=ucs4 --enable-shared"
export PATH="$HOME/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
pyenv virtualenvwrapper_lazy

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
antigen-bundle oknowton/zsh-dwim

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen theme kphoen

# Tell antigen that you're done.
antigen apply

# added by travis gem
[ -f /home/vagrant/.travis/travis.sh ] && source /home/vagrant/.travis/travis.sh

[ -f ~/.aliases ] && source ~/.aliases

# added by travis gem
[ -f ~/.travis/travis.sh ] && source ~/.travis/travis.sh

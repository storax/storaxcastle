HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e

# The following lines were added by compinstall
zstyle :compinstall filename '/home/vagrant/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Develop/Python/
source /usr/local/bin/virtualenvwrapper.sh

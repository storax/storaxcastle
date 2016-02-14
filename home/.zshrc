HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

zstyle :compinstall filename '~/.zshrc'

# added by travis gem
[ -f /home/vagrant/.travis/travis.sh ] && source /home/vagrant/.travis/travis.sh

# added by travis gem
[ -f ~/.travis/travis.sh ] && source ~/.travis/travis.sh

source ~/.zsh/colors.zsh
source ~/.zsh/export.zsh
source ~/.zsh/antigen.zsh
source ~/.zsh/setopt.zsh
source ~/.zsh/prompt.zsh
source ~/.zsh/completion.zsh
source ~/.zsh/alias.zsh
source ~/.zsh/bindkey.zsh
source ~/.zsh/functions.zsh
source ~/.zsh/pyenv.zsh
source ~/.zsh/zaw.zsh
source ~/.zsh/prm.zsh
[ -f ~/.aliases ] && source ~/.aliases

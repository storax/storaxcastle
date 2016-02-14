# Download antigen if it's not there.
[ -f ~/antigen.zsh ] || wget -O ~/antigen.zsh https://raw.githubusercontent.com/zsh-users/antigen/master/antigen.zsh
source ~/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle colored-man-pages
antigen bundle colorize
antigen bundle command-not-found
antigen bundle common-aliases
antigen bundle copydir
antigen bundle copyfile
antigen bundle cp
antigen bundle dirhistory
antigen bundle emacs
antigen bundle gem
antigen bundle git
antigen bundle git-flow
antigen bundle nyan
antigen bundle oknowton/zsh-dwim
antigen bundle pip
antigen bundle python
antigen bundle sudo
antigen bundle vagrant
antigen bundle z
antigen bundle zsh-users/zaw

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions

# Load the theme.
antigen theme kphoen

# Tell antigen that you're done.
antigen apply

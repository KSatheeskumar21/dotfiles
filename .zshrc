# Zsh Config

export TERM="xterm-256color"
export EDITOR="nvim"
export VISUAL="emacsclient -c -a 'emacs'"

# PATH
export PATH="$HOME/.local/bin:$HOME/bin:/usr/bin:$PATH"

# Aliases
alias vim="nvim"
alias ls="exa -lahF --icons"
alias doom="~/.emacs.d/bin/doom"
alias dsync="~/.emacs.d/bin/doom sync"
alias dup="~/.emacs.d/bin/doom sync -u"
alias dupg="~/.emacs.d/bin/doom upgrade"
alias ddoc="~/.emacs.d/bin/doom doctor"

alias config="/usr/bin/git --git-dir=$HOME/dotfiles --work-tree=$HOME"
alias zshconfig="nvim ~/.zshrc && source ~/.zshrc"

alias tobash="sudo chsh $USER -s /bin/bash && echo 'Done...'"
alias tozsh="sudo chsh $USER -s /bin/zsh && echo 'Done...'"
alias tofish="sudo chsh $USER -s /bin/fish && echo 'Done...'"

# Aliases End

# Sourcing Plugins
source ~/.zsh/zsh-autosuggestions.zsh
source ~/.zsh/compleat.plugin.zsh

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/kishore/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Autostart
# pokemon-colorscripts -r
random-script.py
# eval "$(starship init zsh)"

# Setting Editor and Visual variables
set TERM "xterm-256color"
set EDITOR "emacsclient -c -a emacs"
set VISUAL "emacsclient -c -a emacs"

# Fish Vi keybindings
fish_vi_key_bindings

# PATH variable
export PATH="$HOME/bin:/usr/local/bin:/usr/bin:$PATH"

# Setting Manpager
set -x MANPAGER '/bin/bash -c "vim -MRn -c \"set buftype=nofile showtabline=0 ft=man ts=8 nomod nolist norelativenumber nonu noma\" -c \"normal L\" -c \"nmap q :qa<CR>\"</dev/tty <(col -b)"'

# Functions

# Aliases

## Config Aliases
alias fishconfig="$EDITOR ~/.config/fish/config.fish && source ~/.config/fish/config.fish"
alias vimconfig="$EDITOR $HOME/.config/nvim/init.vim"
alias pacmanedit="sudoedit /etc/pacman.conf"

## Set vim to neovim
alias vim="nvim"

## ls alias
alias ls="exa -lahF"

alias code="codium"

## Git Bare repo alias
alias config="/usr/bin/git --git-dir=$HOME/.cfg --work-tree=$HOME"

# Doom alias
alias doom="~/.emacs.d/bin/doom"
alias dsync="~/.emacs.d/bin/doom sync -u"
alias ddoc="~/.emacs.d/bin/doom doctor"
alias dupg="~/.emacs.d/bin/doom upgrade"

## Interactive mode for rm and mv plus backup flag for cp
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -b"

#Leftwm aliases
alias lti="leftwm-theme install"
alias ltu="leftwm-theme uninstall"
alias lta="leftwm-theme apply"
alias ltupd="leftwm-theme update"
alias ltupg="leftwm-theme upgrade"

# Changing shells
alias tobash="sudo chsh $USER -s /bin/bash && echo 'Now log out.'"
alias tozsh="sudo chsh $USER -s /bin/zsh && echo 'Now log out.'"
alias tofish="sudo chsh $USER -s /bin/fish && echo 'Now log out.'"

#switch between lightdm and sddm
alias tolightdm="sudo pacman -S lightdm lightdm-gtk-greeter lightdm-gtk-greeter-settings --noconfirm --needed ; sudo systemctl enable lightdm.service -f ; echo 'Lightm is active - reboot now'"
alias tosddm="sudo pacman -S sddm --noconfirm --needed ; sudo systemctl enable sddm.service -f ; echo 'Sddm is active - reboot now'"

# Neofetch
alias neofetch="neofetch --kitty"

# Startup stuff
set -g fish_greeting
pokemon-colorscripts -r
starship init fish | source

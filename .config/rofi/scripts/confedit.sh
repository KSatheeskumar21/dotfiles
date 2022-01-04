#!/usr/bin/env bash

themesDir="$HOME/.config/rofi/themes/"
scriptsDir="$HOME/.config/rofi/scripts"
rofiCommand="rofi -theme $themesDir/tokyo-night-center-no-rounded-corners.rasi"
editor="emacsclient -c -a 'emacs' "

declare -a configFiles=(
    "Alacritty - $HOME/.config/alacritty/alacritty.yml"
    "Rofi - $HOME/.config/rofi/config.rasi"
    "Xmonad - $HOME/.xmonad/xmonad.hs"
    "Fish - $HOME/.config/fish/config.fish"
    "Fish Prompt - $HOME/.config/fish/functions/fish_prompt.fish"
    "Awesome - $HOME/.config/awesome/rc.lua"
    "Neovim - $HOME/.config/nvim/init.vim"
    "Emacs - $HOME/.emacs.d/config.org"
    "Quit"
)

choice=$(printf '%s\n' "${configFiles[@]}" | $rofiCommand -p "Conf File" -dmenu -selected-row 2)
if [[ $choice == "Quit" ]]; then
    echo "Terminated" && exit 1

elif [[ '$choice' ]]; then
    cfg=$(printf '%s\n' "${choice}" | awk '{print $NF}')
    $editor "$cfg"

else
    echo "Terminated" && exit 1

fi

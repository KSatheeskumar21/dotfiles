#!/usr/bin/env bash

## Copyright (C) 2020-2021 Aditya Shakya <adi1090x@gmail.com>
## Everyone is permitted to copy and distribute copies of this file under GNU-GPL3

DIR="$HOME/.config/"

rofi_command="rofi -theme $DIR/rofi/themes/powermenu.rasi"

uptime=$(uptime -p | sed -e 's/up //g')

# Options
shutdown=""
reboot=""
lock=""
suspend=""
logout=""

# Variable passed to rofi
options="$shutdown\n$reboot\n$lock\n$suspend\n$logout"
_msg="Options  -  yes / y / no / n"

chosen="$(echo -e "$options" | $rofi_command -p "UP - $uptime" -dmenu -selected-row 2)"
case $chosen in
    $shutdown)
		ans=$($HOME/.config/bspwm/rofi/bin/confirm)
		if [[ $ans == "yes" ]] || [[ $ans == "YES" ]] || [[ $ans == "y" ]]; then
        systemctl poweroff
		elif [[ $ans == "no" ]] || [[ $ans == "NO" ]] || [[ $ans == "n" ]]; then
        exit
        else
        rofi -theme ~/.config/bspwm/rofi/themes/askpass.rasi -e "$_msg"
        fi
        ;;
    $reboot)
		ans=$($HOME/.config/bspwm/rofi/bin/confirm)
		if [[ $ans == "yes" ]] || [[ $ans == "YES" ]] || [[ $ans == "y" ]]; then
        systemctl reboot
		elif [[ $ans == "no" ]] || [[ $ans == "NO" ]] || [[ $ans == "n" ]]; then
        exit
        else
        rofi -theme ~/.config/bspwm/rofi/themes/askpass.rasi -e "$_msg"
        fi
        ;;
    $lock)
        bsplock
        ;;
    $suspend)
		ans=$($HOME/.config/bspwm/rofi/bin/confirm)
		if [[ $ans == "yes" ]] || [[ $ans == "YES" ]] || [[ $ans == "y" ]]; then
        mpc -q pause
        amixer set Master mute
        betterlockscreen --suspend
		elif [[ $ans == "no" ]] || [[ $ans == "NO" ]] || [[ $ans == "n" ]]; then
        exit
        else
        rofi -theme ~/.config/bspwm/rofi/themes/askpass.rasi -e "$_msg"
        fi
        ;;
    $logout)
		ans=$($HOME/.config/bspwm/rofi/bin/confirm)
		if [[ $ans == "yes" ]] || [[ $ans == "YES" ]] || [[ $ans == "y" ]]; then
        bspc quit
		elif [[ $ans == "no" ]] || [[ $ans == "NO" ]] || [[ $ans == "n" ]]; then
        exit
        else
        rofi -theme ~/.config/bspwm/rofi/themes/askpass.rasi -e "$_msg"
        fi
        ;;
esac


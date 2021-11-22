#!/usr/bin/env bash

THEMESDIR="$HOME/.config/rofi/themes"

ROFISCRIPTSDIR="$HOME/.config/rofi/scripts"

rofi_command="rofi -theme $THEMESDIR/tokyo-night-dmenu.rasi"

uptime=$(uptime -p | sed -e 's/up //g')

# Power Options
shutdown="Shutdown"
reboot="Reboot"
lock="Lock"
syssuspend="Sleep"
syslogout="Log Out"

# Variable passed to rofi
options="$shutdown\n$reboot\n$lock\n$syssuspend\n$syslogout"
_msg="Options  -  yes / y / no / n"

chosen="$(echo -e "$options" | $rofi_command -p "UP - $uptime" -dmenu -selected-row 2)"
case $chosen in
    $shutdown)
		ans=$($HOME/.config/rofi/scripts/asroot.sh)
		if [[ $ans == "yes" ]] || [[ $ans == "YES" ]] || [[ $ans == "y" ]]; then
        systemctl poweroff
		elif [[ $ans == "no" ]] || [[ $ans == "NO" ]] || [[ $ans == "n" ]]; then
        exit
        else
        rofi -theme $THEMESDIR/askpass.rasi -e "$_msg"
        fi
        ;;
    $reboot)
		ans=$($HOME/.config/rofi/scripts/asroot.sh)
		if [[ $ans == "yes" ]] || [[ $ans == "YES" ]] || [[ $ans == "y" ]]; then
        systemctl reboot
		elif [[ $ans == "no" ]] || [[ $ans == "NO" ]] || [[ $ans == "n" ]]; then
        exit
        else
        rofi -theme $THEMESDIR/askpass.rasi -e "$_msg"
        fi
        ;;
    $lock)
        betterlockscreen -l
        ;;
    $suspend)
		ans=$($HOME/.config/rofi/scripts/asroot.sh)
		if [[ $ans == "yes" ]] || [[ $ans == "YES" ]] || [[ $ans == "y" ]]; then
        mpc -q pause
        amixer set Master mute
        betterlockscreen --suspend
		elif [[ $ans == "no" ]] || [[ $ans == "NO" ]] || [[ $ans == "n" ]]; then
        exit
        else
        rofi -theme $THEMESDIR/askpass.rasi -e "$_msg"
        fi
        ;;
    $logout)
		ans=$($HOME/.config/rofi/scripts/asroot.sh)
		if [[ $ans == "yes" ]] || [[ $ans == "YES" ]] || [[ $ans == "y" ]]; then
        systemctl exit
		elif [[ $ans == "no" ]] || [[ $ans == "NO" ]] || [[ $ans == "n" ]]; then
        exit
        else
        rofi -theme ~/.config/rofi/themes/askpass.rasi -e "$_msg"
        fi
        ;;
esac


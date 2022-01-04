#!/usr/bin/env bash

THEMEDIR="$HOME/.config/rofi/themes"

yes="Yes"
no="No"

options="$yes\n$no"

rofi_cmd=$(echo -e "$options" | rofi -dmenu -i -no-fixed-num-lines -p "Are you sure? : " -theme $THEMEDIR/tokyo-night-center-no-rounded-corners.rasi)

case $rofi_cmd in
	$yes)
		echo "yes"
		;;
	$no)
		echo "no"
		;;
esac

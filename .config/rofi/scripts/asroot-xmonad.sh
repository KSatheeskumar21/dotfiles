#!/usr/bin/env bash

THEMEDIR="$HOME/.config/rofi/themes"

rofi -dmenu \
	-i \
	-no-fixed-num-lines \
	-p "Are you sure? : " \
	-theme $THEMEDIR/tokyo-night-center.rasi 

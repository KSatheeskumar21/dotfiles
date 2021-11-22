#!/usr/bin/env bash

THEMESDIR="$HOME/.config/rofi/themes"

rofi \
	-modi drun,run,combi,ssh \
	-combi-modi drun,run \
	-show combi \
	-no-lazy-grab \
	-scroll-method 0 \
	-drun-match-fields all \
	-terminal alacritty \
	-display-combi "Run:" \
	-theme "$THEMESDIR/tokyo-night-dmenu.rasi"

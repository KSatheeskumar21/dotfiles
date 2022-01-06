#!/usr/bin/env bash
# shellcheck disable=SC2154

ROFICMD=("rofi -dmenu -p")
setbg_dir="${HOME}/Pictures/Wallpapers"

# Set with the flags "-e", "-u","-o pipefail" cause the script to fail
# if certain things happen, which is a good thing.  Otherwise, we can
# get hidden bugs that are hard to discover.
set -euo pipefail
use_imv=0

# Set wallpaper using either xwallpaper or swaybf
setbg() {
  case "$XDG_SESSION_TYPE" in
    'x11') xwallpaper --stretch "$1";;
    'wayland') swaybg -m "stretch" -i "$1";;
    *) err "Unknown display server";;
  esac
}

main() {
  choice="$(printf "Set\nRandom\nExit" | ${ROFICMD} "What would you like to do?")"
  case "$choice" in
    "Set")
    if [ "$use_imv" = 0 ]; then
      wallpaper="$(sxiv -t -o "${setbg_dir}")"
      echo "$wallpaper" > "$HOME"/.cache/wall
      setbg "$wallpaper"
    else
      # Read each line printed to stdin, imv behaves weird
      # We also ignore a shellcheck issue because it's a bug
      # shellcheck disable=SC2154
      imv "${setbg_dir}" | while read -r LINE; do
        pidof "swaybg" && killall "swaybg"
        pidof "xwallpaper" && killall "xwallpaper"
        setbg "$LINE" & # Sets the wallpaper and frees it from the process
        notify-send "Wallpaper has been updated" "$LINE is the current wallpaper, edit your window manager config if you would like this to persist on reboot"
      done
    fi
    ;;
    "Random")
    valid_paper="No"
    until [ "$valid_paper" = "Yes" ]; do
      pidof "swaybg" && killall "swaybg"
      pidof "xwallpaper" && killall "xwallpaper"
      wallpaper="$(find "${setbg_dir}" -type f | shuf -n 1)"
      setbg "$wallpaper" &
      cp "$wallpaper" "$HOME/.cache/wall"
      valid_paper="$(printf "Yes\nNo" | ${ROFICMD} "Do you like the new wallpaper?")"
    done
    ;;
    "Exit") echo "Program terminated" && exit 1;;
    *) err "Invalid choice";;
  esac
}

[[ "${BASH_SOURCE[0]}" == "${0}" ]] && main "$@"
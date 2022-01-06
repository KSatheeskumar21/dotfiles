#!/usr/bin/env bash

set -euo pipefail

maim_dir="${HOME}/Screenshots"

get_timestamp() {
    date '+%Y%M%d-%H%M%S'
}

copy() {
    case "$XDG_SESSION_TYPE" in
        'x11') xclip -selection clipboard -t image/png;;
        'wayland') wl-copy -t image/png;;
        *) err "Unknown display server";;
    esac
}

main() {
    local _maim_args=""
    local _file_type=""

    # shellcheck disable=SC2154
    mkdir -p "${maim_dir}"
}

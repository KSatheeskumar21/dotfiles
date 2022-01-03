#!/usr/bin/env bash

set -euo pipefail

SCREENDIR="$HOME/screen-scripts/"

screen -d -m $SCREENDIR/ping-google.sh

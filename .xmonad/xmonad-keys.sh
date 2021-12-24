#!/usr/bin/env bash

set -euo pipefail

sed -n '/KB_BEGIN/,/KB_END/p' ~/.xmonad/xmonad.hs | \
	grep -e ', ("' \
	-e '\[ ('  \
	-e 'KB_GROUP' | \
	grep -v '\-\- , ("' | \
	sed -e 's/^[ \t]*//' \
	-e 's/, (/(/' \
	-e 's/\[ (/(/' \
	-e 's/-- KB_GROUP /\n/' \
	-e 's/", /" -> /' | \
	yad --text-info

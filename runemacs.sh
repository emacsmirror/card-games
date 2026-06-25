#!/usr/bin/env bash
# Load the card-games package from this directory and open the launcher.
dir="$(cd "$(dirname "$0")" && pwd)"
exec emacs -L "$dir" -l card-games -f card-game "$@"

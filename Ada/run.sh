#!/bin/sh
set -eu

program=$(basename "$1" .adb)
gnatmake -o "$program" "$1" && printf '\n' && exec "./$program"

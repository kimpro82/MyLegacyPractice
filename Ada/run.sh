#!/bin/sh
set -eu

program=$(basename "$1" .adb)
gnatmake -o "$program" "$1" && exec "./$program"

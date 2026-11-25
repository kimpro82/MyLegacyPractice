#!/bin/sh
set -eu

command -v gnatmake >/dev/null 2>&1 && exit 0

if [ "$(id -u)" -eq 0 ]; then
    apt-get update && apt-get install -y gnat
else
    sudo -A apt-get update && sudo -A apt-get install -y gnat
fi

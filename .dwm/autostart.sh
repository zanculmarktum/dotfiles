#!/bin/sh

hsetroot -center $(cd "${0%/*}"; printf "$(pwd)"'/%s\n' wall.* | head -1) &
redshift-gtk -P &
parcellite &
conky | while read line; do xsetroot -name "$line"; done &

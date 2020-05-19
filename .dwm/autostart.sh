#!/bin/sh

hsetroot -center $(cd "${0%/*}"; printf "$(pwd)"'/%s\n' wall.* | head -1) &
redshift-gtk &
parcellite &
conky | while read line; do xsetroot -name "$line"; done &
xautolock -time 5 -locker 'i3lock-fancy & { sleep 300; systemctl hibernate; }' -corners 000- -cornersize 30 &

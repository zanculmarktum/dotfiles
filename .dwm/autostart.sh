#!/bin/sh

hsetroot -fill $(cd "${0%/*}"; printf "$(pwd)"'/%s\n' wall.* | head -1) &
redshift-gtk &
parcellite &
compton &
conky | while read line; do xsetroot -name "$line"; done &
#xautolock -time 5 -locker 'i3lock-fancy &
#i=1
#while :; do
#  sleep 10
#  if ! pidof i3lock >/dev/null 2>&1; then
#    break
#  fi
#  if [ $i -eq 30 ]; then
#    systemctl hibernate
#    break
#  fi
#  i=$(($i+1))
#done' -corners 000- -cornersize 30 &

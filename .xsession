#!/bin/sh

if [ "x${PATH#*/usr/local/bin}" != "x${PATH}" ]; then
  export PATH=/usr/local/bin:$PATH
fi

# Set the keyboard layout
setxkbmap -layout jp,us

xmodmap $HOME/.Xmodmap

# Fonts
#xset +fp /usr/share/fonts/cantarell/
#xset fp rehash

# Fix xmonad "X" cursor issue
xsetroot -cursor_name left_ptr

# Siji font
#xset +fp $HOME/.local/share/fonts
#xset fp rehash

# Disable beeping
xset b off

if grep -qe 'ThinkPad' /sys/devices/virtual/dmi/id/product_version; then
  # TrackPoint
  xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 1
  xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 2
  xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 200
  xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 6 7 4 5

  # TouchPad
  xinput set-prop "SynPS/2 Synaptics TouchPad" "Device Enabled" 0
fi

# Starts xbindkeys
xbindkeys

# Autostart the apps
xdg-autostart -d Old

# Starts the party
session=${1:-awesome}

case $session in
  i3|i3wm)
    # Fake running DE to Qt5
    #export XDG_CURRENT_DESKTOP=GNOME
    #export QT_STYLE_OVERRIDE=gtk
    export QT_QPA_PLATFORMTHEME=qt5ct

    #compton -b &
    # feh --bg-fill "/figment/wall/RkJ5hpW.png" &
    i3
    ;;
  openbox)
    openbox-session > /dev/null 2>&1
    ;;
  2bwm)
    ~/.config/2bwm/autostart.sh
    ;;
  awesome)
    awesome
    ;;
  fvwm)
    fvwm
    ;;
  echinus)
    echinus
    ;;
  frankenwm)
    frankenwm
    ;;
  monsterwm)
    monsterwm
    ;;
  dwm)
    dwm
    ;;
  wmii)
    wmii
    ;;
  subtle)
    subtle
    ;;
  plasma|kde)
    startkde
    ;;
  icewm)
    icewm
    ;;
  herbstluftwm)
    herbstluftwm
    ;;
  twm)
    twm
    ;;
  xmonad)
    xmonad
    ;;
  stumpwm)
    # sbcl --non-interactive --load $HOME/stumpwm-24.11/foo.lisp
    sbcl --non-interactive \
         --eval "(push \"/home/azure/stumpwm-24.11/\" asdf:*central-registry*)" \
         --eval "(asdf:operate 'asdf:load-op ':stumpwm)" \
         --eval "(stumpwm:stumpwm)"
    ;;
  # No known session, try to run it as command
  *) $1;;
esac

# vim:ts=2:sw=2:et

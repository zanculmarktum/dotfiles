#!/bin/sh
# $Xorg: xinitrc.cpp,v 1.3 2000/08/17 19:54:30 cpqbld Exp $

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps
[ -f $sysresources ] && /usr/bin/xrdb -merge $sysresources
[ -f $sysmodmap ] && /usr/bin/xmodmap $sysmodmap
[ -f $userresources ] && /usr/bin/xrdb -merge $userresources
[ -f $usermodmap ] && /usr/bin/xmodmap $usermodmap

# Fonts
#xset +fp /usr/share/fonts/cantarell/
#xset fp rehash

# Fix xmonad "X" cursor issue
#xsetroot -cursor_name left_ptr

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

# Starts the party
session=${1:-dwm}

case $session in
  i3|i3wm)
    # Fake running DE to Qt5
    #export XDG_CURRENT_DESKTOP=GNOME
    #export QT_STYLE_OVERRIDE=gtk
    export QT_QPA_PLATFORMTHEME=qt5ct
  
    #compton -b &
    feh --bg-fill "/figment/wall/RkJ5hpW.png" &
    exec i3
    ;;
  openbox)
    if [ -z "$DESKTOP_SESSION" -a -x /usr/bin/ck-launch-session ]; then
      ck-launch-session dbus-launch --exit-with-session openbox-session > /dev/null 2>&1
    else
      openbox-session > /dev/null 2>&1
    fi
    ;;
  2bwm)
    ck-launch-session dbus-launch --exit-with-session ~/.config/2bwm/autostart.sh
    ;;
  awesome)
    ck-launch-session dbus-launch --exit-with-session awesome
    ;;
  fvwm)
    ck-launch-session dbus-launch --exit-with-session fvwm
    ;;
  echinus)
    ck-launch-session dbus-launch --exit-with-session echinus
    ;;
  frankenwm)
    ck-launch-session dbus-launch --exit-with-session frankenwm
    ;;
  monsterwm)
    ck-launch-session dbus-launch --exit-with-session monsterwm
    ;;
  dwm)
    if [ -f /etc/slackware-version ]; then
      exec ck-launch-session dwm
    else
      exec dwm
    fi
    ;;
  wmii)
    ck-launch-session dbus-launch --exit-with-session wmii
    ;;
  subtle)
    ck-launch-session dbus-launch --exit-with-session subtle
    ;;
  plasma|kde)
    ck-launch-session dbus-launch --exit-with-session startkde
    ;;
  icewm)
    ck-launch-session dbus-launch --exit-with-session icewm
    ;;
  herbstluftwm)
    ck-launch-session dbus-launch --exit-with-session herbstluftwm
    ;;
  twm)
    ck-launch-session dbus-launch --exit-with-session twm
    ;;
  # No known session, try to run it as command
  *) exec $1;;
esac

# vim:ts=2:sw=2:et

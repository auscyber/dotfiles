#!/bin/sh
killall -q polybar
eww reload
eww daemon
polybar desktopbar --config=~/.config/polybar/config.ini 2>&1 | tee -a /tmp/polybar.log & disown

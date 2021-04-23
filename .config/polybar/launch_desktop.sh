#!/bin/sh
pkill polybar 
~/eww/target/debug/eww reload
~/eww/target/debug/eww daemon
polybar desktopbar --config=~/.config/polybar/config.ini &


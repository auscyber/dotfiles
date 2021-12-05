#!/bin/sh
pkill polybar
eww reload
eww daemon
polybar desktopbar --config=~/.config/polybar/config.ini &


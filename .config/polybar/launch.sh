#!/bin/sh
pkill polybar 
~/eww/target/debug/eww reload
~/eww/target/debug/eww daemon
polybar mybar --config=~/.config/polybar/config.ini &


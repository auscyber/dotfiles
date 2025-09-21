#!/bin/sh


POWER="$(pmset -g ac | grep -Eo "Wattage = (\d+W)" | cut -d= -f 2 | tr -d '[:space:]')"



if [[ -n $POWER ]]; then
sketchybar --set "$NAME"  label="$POWER" drawing=on icon.drawing=off
else
sketchybar --set "$NAME" drawing=off icon.drawing=off
fi

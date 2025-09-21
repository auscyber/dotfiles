#!/bin/sh

# The $NAME variable is passed from sketchybar and holds the name of
# the item invoking this script:
# https://felixkratz.github.io/SketchyBar/config/events#events-and-scripting

# open notification center on click
sketchybar --set "$NAME" label="$(date '+%d/%m %I:%M %p')" click_script="open -a 'Notification Center'"



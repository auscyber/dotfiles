#!/usr/bin/env sh

# Format time in a style similar to polybar
time=$(date +"%I:%M%p" | tr '[:upper:]' '[:lower:]')

sketchybar --set $NAME label="$time" \
                      background.color=0xff3f3f3f \
                      label.color=0xffffffff
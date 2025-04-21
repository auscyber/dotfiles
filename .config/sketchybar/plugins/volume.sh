#!/usr/bin/env sh

# Get volume percentage
volume=$(osascript -e "output volume of (get volume settings)")
muted=$(osascript -e "output muted of (get volume settings)")

if [ "$muted" = "true" ]; then
  icon=""
  label="muted"
else
  if [ "$volume" -gt 66 ]; then
    icon="墳"
  elif [ "$volume" -gt 33 ]; then
    icon="墳"
  else
    icon="奄"
  fi
  label="$volume%"
fi

sketchybar --set $NAME icon="$icon" \
                      label="$label" \
                      icon.color=0xff8BB2C1 \
                      label.color=0xff8BB2C1

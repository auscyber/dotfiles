#!/usr/bin/env sh

# This is a simplified version - you'll need to integrate with your window manager
# For now, we'll create a simple space indicator

spaces=$(yabai -m query --spaces | jq -r '.[] | select(."is-visible" == true).index')
active=$(yabai -m query --spaces | jq -r '.[] | select(."has-focus" == true).index')

workspace_list=""
for i in {1..5}; do
  if [ "$i" = "$active" ]; then
    workspace_list+=" "
  elif echo "$spaces" | grep -q "$i"; then
    workspace_list+=" $i"
  else
    workspace_list+=" $i"
  fi
done

sketchybar --set $NAME label="$workspace_list"
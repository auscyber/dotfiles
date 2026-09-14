#!/usr/bin/env sh

# Get uptime in hours
uptime_output=$(uptime)
uptime_hours=$(echo "$uptime_output" | awk -F" " '{ print $3}' | sed 's/,//')

sketchybar --set $NAME label=" $uptime_hours" \
                      icon=""
#!/usr/bin/env bash

# The $SELECTED variable is available for space components and indicates if
# the space invoking this script (with name: $NAME) is currently selected:
# https://felixkratz.github.io/SketchyBar/config/components#space----associate-mission-control-spaces-with-an-item

source "$HOME/.config/icon_map.sh"



APP_ICON_LIST=""
WORKSPACE_ID=${1:-${NAME#space.}}

RIFT_SPACE_ID=$(echo "$WORKSPACE_ID" | sed 's/__/ /g') # Space with full name

apps=$(rift-cli query workspaces | jq -r ".[] | select(.name == \"$RIFT_SPACE_ID\") | .windows[].bundle_id" | sort -u)

FOCUSED_WORKSPACE=$(rift-cli query workspaces | jq -r '.[] | select(.is_active == true) | .name')
if [ "$FOCUSED_WORKSPACE" = "$RIFT_SPACE_ID" ]; then
	SELECTED="true"
else
	SELECTED="false"
fi

while read -r app; do
			icon_strip+=" $(
				__icon_map "$app"
				echo $icon_result
			)"
		done <<<"${apps}"

if [ -z "$icon_strip" ]; then
LABEL_DRAWING="off"
else
LABEL_DRAWING="on"
fi

case "$SENDER" in
	"mouse.clicked")
		rift-cli execute workspace switch  "${NAME#space.}"
		;;
	"rift_windows_changed")
		;;
esac

sketchybar --set "$NAME" background.drawing="$SELECTED" icon.drawing=off label="$icon_strip" label.drawing="$LABEL_DRAWING" label.border_color=0xff$COLOR_FOREGROUND icon.drawing=off label.padding_left=1 label.border_width=1

#!/usr/bin/env bash

if [ -r "$CONFIG_DIR/colors.sh" ]; then
	source "$CONFIG_DIR/colors.sh"
fi

: "${COLOR_FOREGROUND:=e6e8ef}"
color_foreground="${COLOR_FOREGROUND#0x}"
color_foreground="${color_foreground#\#}"
if [ "${#color_foreground}" -eq 8 ]; then
	color_foreground="${color_foreground:2}"
fi
if ! printf '%s' "$color_foreground" | grep -Eq '^[0-9A-Fa-f]{6}$'; then
	color_foreground="e6e8ef"
fi

# The $SELECTED variable is available for space components and indicates if
# the space invoking this script (with name: $NAME) is currently selected:
# https://felixkratz.github.io/SketchyBar/config/components#space----associate-mission-control-spaces-with-an-item

source "$HOME/.config/icon_map.sh"

ITEM_NAME=${2:-${NAME}}

case "$SENDER" in
	"mouse.clicked")
		rift-cli execute workspace switch "${ITEM_NAME#space.}"
		exit 0
		;;
	"mouse.entered"|"mouse.exited")
		# Hover events do not need full workspace refreshes.
		exit 0
		;;
esac

WORKSPACE_ID=${1:-${ITEM_NAME#space.}}
RIFT_SPACE_ID=$(echo "$WORKSPACE_ID" | sed 's/__/ /g')

workspace_cache_file="/tmp/rift-workspaces.json"
workspace_cache_ttl=1

now_ts=$(date +%s)
cache_ts=0
if [ -f "$workspace_cache_file" ]; then
	cache_ts=$(stat -f %m "$workspace_cache_file" 2>/dev/null || echo 0)
fi

workspace_json=""
if [ -f "$workspace_cache_file" ] && [ $((now_ts - cache_ts)) -lt "$workspace_cache_ttl" ]; then
	workspace_json=$(cat "$workspace_cache_file" 2>/dev/null)
fi

if [ -z "$workspace_json" ]; then
	workspace_json=$(rift-cli query workspaces 2>/dev/null)
	if [ -n "$workspace_json" ]; then
		printf "%s" "$workspace_json" >"$workspace_cache_file"
	fi
fi

if [ -z "$workspace_json" ]; then
	exit 0
fi

apps=$(printf "%s" "$workspace_json" | jq -r --arg name "$RIFT_SPACE_ID" '.[] | select(.name == $name) | .windows[].app_name' | sort -u)
FOCUSED_WORKSPACE=$(printf "%s" "$workspace_json" | jq -r '.[] | select(.is_active == true) | .name')

if [ "$FOCUSED_WORKSPACE" = "$RIFT_SPACE_ID" ]; then
	SELECTED="true"
else
	SELECTED="false"
fi

icon_strip=""
while read -r app; do
	icon_strip+=" $(
		__icon_map "$app"
		echo "$icon_result"
	)"
done <<<"${apps}"

if [ -z "$icon_strip" ]; then
	LABEL_DRAWING="off"
else
	LABEL_DRAWING="on"
fi

sketchybar --set "$ITEM_NAME" \
	background.drawing="$SELECTED" \
	icon.drawing=off \
	label="$icon_strip" \
	label.drawing="$LABEL_DRAWING" \
	label.border_color=0xff$color_foreground \
	label.padding_left=1 \
	label.border_width=1

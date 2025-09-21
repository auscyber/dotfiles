#!/usr/bin/env bash

# The $SELECTED variable is available for space components and indicates if
# the space invoking this script (with name: $NAME) is currently selected:
# https://felixkratz.github.io/SketchyBar/config/components#space----associate-mission-control-spaces-with-an-item


APP_NAMES="$(yabai -m query --windows --space "$SID" | jq '.[] | select(.scratchpad | test("^$") ) | .app')"


APP_ICON_LIST=""


for APP_NAME in $APP_NAMES; do
    APP_NAME=$(echo "$APP_NAME" | tr -d '"')
    case "$APP_NAME" in
        "Code") APP_ICON_LIST+="󰨞 " ;;
        "Ghostty") APP_ICON_LIST+=" " ;;
        "Finder") APP_ICON_LIST+="󰀶 " ;;
        "Fantastical") APP_ICON_LIST+="󰃰 " ;;
        "Beeper" | "Messages" ) APP_ICON_LIST+="󰭹 " ;;
        "Zen") APP_ICON_LIST+="󰖟 " ;;
        "Slack") APP_ICON_LIST+="󰒱 " ;;
        
        *) APP_ICON_LIST+="" ;;
    esac
done

if [ -z "$APP_ICON_LIST" ]; then
LABEL_DRAWING="off"
else
LABEL_DRAWING="on"
fi


sketchybar --set "$NAME" background.drawing="$SELECTED" icon="$SID"  label="$APP_ICON_LIST" label.drawing="$LABEL_DRAWING" label.border_color=0xff$COLOR_FOREGROUND

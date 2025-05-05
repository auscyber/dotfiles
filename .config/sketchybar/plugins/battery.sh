#!/usr/bin/env sh

# Get battery percentage and charging status
PERCENTAGE=$(pmset -g batt | grep -Eo "\d+%" | cut -d% -f1)
CHARGING=$(pmset -g batt | grep 'AC Power')

if [ "$CHARGING" != "" ]; then
    # Charging - show charging icon
    case ${PERCENTAGE} in
        100) ICON=" 󱐋";;
		9[0-9]) ICON=" 󱐋";;
        [6-8][0-9]) ICON=" 󱊥󱐋" ;;
        [3-5][0-9]) ICON=" 󱊤󱐋" ;;
        [1-2][0-9]) ICON=" 󰢟󱐋" ;;
        *) ICON=" 󱐋" ;;
    esac
else
    # Discharging - show battery level icon
    case ${PERCENTAGE} in
        9[0-9]|100) ICON=" " COLOR=0xff82FFA2 ;;
        [6-8][0-9]) ICON=" " COLOR=0xff82FFA2 ;;
        [3-5][0-9]) ICON=" " COLOR=0xffFFFFFF ;;
        [1-2][0-9]) ICON="" COLOR=0xffFFAE8F ;;
        *) ICON=" " COLOR=0xffFF6970 ;;
    esac
fi

sketchybar --set $NAME icon="$ICON" label="${PERCENTAGE}%" icon.color=${COLOR:-0xffffffff}

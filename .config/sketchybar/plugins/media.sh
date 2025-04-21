#!/usr/bin/env sh

# This gets the currently playing track from Music/Spotify
PLAYER_STATE=$(osascript -e 'tell application "System Events"
    set processList to name of every process
    if processList contains "Music" then
        tell application "Music"
            if player state is playing then
                set trackName to name of current track
                set artistName to artist of current track
                return "♫ " & trackName & " - " & artistName
            else
                return ""
            end if
        end tell
    else if processList contains "Spotify" then
        tell application "Spotify"
            if player state is playing then
                set trackName to name of current track
                set artistName to artist of current track
                return "♫ " & trackName & " - " & artistName
            else
                return ""
            end if
        end tell
    else
        return ""
    end if
end tell')

if [ "$PLAYER_STATE" = "" ]; then
  sketchybar --set $NAME label=""
else
  sketchybar --set $NAME label="$PLAYER_STATE"
fi
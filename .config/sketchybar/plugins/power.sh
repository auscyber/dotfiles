#!/usr/bin/env sh

# Handle power menu click
osascript -e 'tell application "System Events" to display dialog "Power Options" buttons {"Sleep", "Restart", "Shutdown", "Cancel"} default button "Cancel" with title "Power Menu"' > /tmp/power_choice_result 2>&1

choice=$(cat /tmp/power_choice_result | grep "button returned" | awk -F":" '{print $2}' | tr -d ' ')

case "$choice" in
  "Sleep")
    osascript -e 'tell application "System Events" to sleep'
    ;;
  "Restart")
    osascript -e 'tell application "System Events" to restart'
    ;;
  "Shutdown")
    osascript -e 'tell application "System Events" to shut down'
    ;;
  *)
    # Cancel or no selection
    ;;
esac
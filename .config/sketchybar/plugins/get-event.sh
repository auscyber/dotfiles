#!/usr/bin/env bash
eventtext="$(shortcuts run "Upcoming Item Text" --output-type public.plain-text | cat)"
readarray -t array <<< $eventtext

echo ${array[0]}


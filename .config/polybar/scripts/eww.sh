#!/usr/bin/env bash
EWW_PATH=~/eww/target/debug/eww
if $EWW_PATH windows | grep -Eq "\*$1$"; then
   $EWW_PATH close $1
else
    $EWW_PATH open $1
fi

#!/bin/sh
P="$HOME/.face"
if [[ -n $1 ]]; then
    P=$1
fi

dbus-send --system --dest=org.freedesktop.Accounts --type=method_call --print-reply=literal /org/freedesktop/Accounts/User1000 org.freedesktop.Accounts.User.SetIconFile string:'$P'

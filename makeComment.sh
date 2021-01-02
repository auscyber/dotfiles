#!/bin/sh
figlet -f slant "$2" | sed -E "s/^/$1/gm;t;d"


#!/bin/sh
pacman -Qen > "sync_packages"
pacman -Qem > "aur_packages"

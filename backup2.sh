#!/bin/sh
set -e
DEST=~/dotfiles
cd $DEST
sync () {
    if [[ -n  $3 ]]; then
        rsync -avhr "$1" "$DEST/$2"  --exclude $3 --delete
    else
        rsync -avhr "$1" "$DEST/$2" --delete
    fi
}

{
    #Test Distro
eval "$(cat /etc/os-release | head)"
dis=$NAME
if grep -q 'Gentoo' <<< "$dis"; then
    sync /etc/portage/ portage/etc/ 
    sync /var/lib/portage/world portage/world
    cp /usr/src/linux/.config $DEST/kernel/
elif grep -q 'NixOS' <<< "$dis" ; then
    sync /etc/nixos/ nixos/config
    sync ~/.config/nixpkgs/ .config/nixpkgs
fi
if [ ! grep -q 'NixOS' <<< "$dis" ] ; then
    sync ~/st rice  .git
    sync ~/.config/polybar .config 
    sync ~/.config/nvim .config 
    sync ~/.config/alacritty/alacritty.yml .config 
    sync ~/.config/picom .config 
fi

sync ~/.xmonad rice  {.venv,*.o}
sync ~/st rice  .git
sync ~/.config/starship.toml .config
sync ~/.config/polybar .config 
sync ~/.config/rofi .config 
cp ~/.xinitrc $DEST/rice/
cp ~/.Xresources $DEST/rice/
cd ~/dotfiles 
git add --all
git commit -a -m "$(date "+%a %d %b %I:%M%P") backup" 
git push origin master -q
}   > /dev/null

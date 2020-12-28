#!/bin/sh
cd "$(basename $0)"
sync () {
    if [[ -n  $3 ]]; then
        rsync -avhr "$1" "$DEST/$2"  --exclude $3 --delete
    else
        rsync -avhr "$1" "$DEST/$2" --delete
    fi
}

{
    #Test Distro
dis="$(cat /etc/os-release)"
if grep -q 'Gentoo' <<< "$dis"; then
    sync /etc/portage/ portage/etc/ 
    sync /var/lib/portage/world portage/world
    cp /usr/src/linux/.config $DEST/kernel/
elif grep -q 'NixOS' <<< "$dis" ; then
    sync /etc/nixos/ nixos/config
    sync ~/.config/nixpkgs/ .config/nixos
fi

sync ~/.xmonad rice  {.venv,*.o}
sync ~/st rice  .git
sync ~/.config/alacritty/alacritty.yml .config 
sync ~/.config/starship.toml .config
sync ~/.config/polybar .config 
sync ~/.config/nvim .config 
sync ~/.config/picom .config 
sync ~/.config/rofi .config 
sync ~/picom rice 
cp ~/.xinitrc $DEST/rice/
cp ~/.Xresources $DEST/rice/
cp ~/.zshrc $DEST
cd ~/dotfiles 
git add --all
git commit -a -m "$(date "+%a %d %b %I:%M%P") backup" 
git push origin master -q
}   > /dev/null

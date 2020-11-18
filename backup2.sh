HOME=/home/auscyber
DEST=~/dotfiles 
cp -r /etc/portage $DEST
cp /usr/src/linux/.config $DEST/kernel
#zip -r ~/dotfiles/rice.zip  {~/.xmonad,~/st,~/dmenu,~/.xinitrc,~/.Xresources}
cp -r ~/.xmonad $DEST
cp -r ~/st $DEST
cp ~/.xinitrc $DEST 
cp ~/.Xresources $DEST
cp -r ~/.config/polybar $DEST
cp -r ~/.config/nvim $DEST
cd ~/dotfiles
git add ./*
git commit -a -m "$(date "+%a %d %b %I:%M%P") backup"
git push origin


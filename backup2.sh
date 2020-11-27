HOME=/home/auscyber
DEST=~/dotfiles 
rsync -avhr /etc/portage/ portage/etc/ --delete
rsync -avh /var/lib/portage/world portage/world --delete
cp /usr/src/linux/.config $DEST/kernel/
#zip -r ~/dotfiles/rice.zip  {~/.xmonad,~/st,~/dmenu,~/.xinitrc,~/.Xresources}
rsync -avhr ~/.xmonad $DEST/rice --delete --exclude *.o
rsync -avhr ~/st $DEST/rice --delete --exclude .git
rsync -avhr ~/.config/polybar $DEST/rice --delete
rsync -avhr ~/.config/nvim $DEST --delete
rsync -avhr ~/.config/picom $DEST --delete
rsync -avhr ~/.config/rofi $DEST/rice --delete
cp ~/.xinitrc $DEST/rice/
cp ~/.Xresources $DEST/rice/
cp ~/.zshrc $DEST
cd ~/dotfiles
git add -all --exclude *.o
git commit -a -m "$(date "+%a %d %b %I:%M%P") backup"
git push origin master


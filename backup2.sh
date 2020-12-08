HOME=/home/auscyber
DEST=~/dotfiles 
rsync -avhr /etc/portage/ $DEST/portage/etc/ --delete
rsync -avh /var/lib/portage/world $DEST/portage/world --delete
cp /usr/src/linux/.config $DEST/kernel/
#zip -r ~/dotfiles/rice.zip  {~/.xmonad,~/st,~/dmenu,~/.xinitrc,~/.Xresources}
rsync -avhr ~/.xmonad $DEST/rice --delete --exclude *.o
rsync -avhr ~/st $DEST/rice --delete --exclude .git
rsync -avhr ~/.config/polybar $DEST/.config --delete
rsync -avhr ~/.config/nvim $DEST/.config --delete
rsync -avhr ~/.config/picom $DEST/.config --delete
rsync -avhr ~/.config/rofi $DEST/.config --delete
rsync -avhr ~/picom $DEST/rice --delete
cp ~/.xinitrc $DEST/rice/
cp ~/.Xresources $DEST/rice/
cp ~/.zshrc $DEST
cd ~/dotfiles
git add --all
git commit -a -m "$(date "+%a %d %b %I:%M%P") backup"
git push origin master

#rsync -av -e ssh ~/dotfiles admin@192.168.1.33:NetBackup

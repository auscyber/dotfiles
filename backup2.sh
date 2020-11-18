HOME=/home/auscyber
DEST=~/dotfiles 
cp -r /etc/portage $DEST
rsync -avhr /etc/portage/ portage/ --delete
cp /usr/src/linux/.config $DEST/kernel/
#zip -r ~/dotfiles/rice.zip  {~/.xmonad,~/st,~/dmenu,~/.xinitrc,~/.Xresources}
rsync -avhr ~/.xmonad $DEST --delete
rsync -avhr ~/st $DEST --delete
rsync -avhr ~/.config/polybar $DEST --delete
rsync -avhr ~/.config/nvim $DEST --delete
rsync -avhr ~/st $DEST --delete
rsync -avhr ~/st $DEST --delete
cp ~/.xinitrc $DEST 
cp ~/.Xresources $DEST
cd ~/dotfiles
git add ./*
git commit -a -m "$(date "+%a %d %b %I:%M%P") backup"
git push origin master


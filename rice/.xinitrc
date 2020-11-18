[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources &
feh --bg-scale ~/bg.jpeg &
exec dbus-launch --sh-syntax --exit-with-session xmonad 

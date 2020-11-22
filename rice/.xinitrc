[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources &
feh --bg-scale ~/bg.jpeg &
xseteroot -cursor_name left_ptr
exec dbus-launch --sh-syntax --exit-with-session xmonad 

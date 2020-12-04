[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources &
feh --bg-center ~/background\ 2.jpg &
xseteroot -cursor_name left_ptr
exec dbus-launch --sh-syntax --exit-with-session xmonad 

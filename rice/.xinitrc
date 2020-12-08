[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources &
xseteroot -cursor_name left_ptr
exec dbus-launch --sh-syntax --exit-with-session xmonad 

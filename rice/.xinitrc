[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources &
#xseteroot -cursor_name left_ptr
source ~/.zshrc
exec dbus-run-session xmonad

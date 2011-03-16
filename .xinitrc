trayer --edge top --align right --SetDockType true --SetPartialStrut true \
    --expand true --width 10 --transparent true --tint 0x191970 --height 12&

if test "$(hostname)" = zigon* ; then
	wicd-gtk&
	transmission --minimized &
fi

emacs --daemon &
xmodmap ~/dotfiles/neo_de.xmodmap
xset b off
exec xmonad

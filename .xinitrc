trayer --edge top --align right --SetDockType true --SetPartialStrut true \
    --expand true --width 10 --transparent true --tint 0x191970 --height 12&

if test "$(hostname)" = "jubold"; then
	xmodmap -e "keycode 166 = Super_R"
	wicd-client -t &
	pidgin &
	linphone-3 --iconified &
	transmission -m &
fi

xmodmap ~/dotfiles/neo_de.xmodmap
xset b off
exec xmonad

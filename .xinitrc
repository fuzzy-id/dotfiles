trayer --edge top --align right --SetDockType true --SetPartialStrut true \
    --expand true --width 10 --transparent true --tint 0x191970 --height 12&

if test "$(hostname)" = "jubold"; then
	wicd-gtk&
	xmodmap -e "keycode 166 = Super_R"
fi

xmodmap ~/dotfiles/neo_de.xmodmap
xset b off
exec xmonad

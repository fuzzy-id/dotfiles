# ~/.xinitrc
# This trayer is for xmonad + xmobar and holds things as transmission, wicd, etc.
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
    --expand true --width 10 --transparent true --tint 0x191970 --height 12&

# here comes machine dependent stuff, as I currently only have one
# machine, this is pretty straight forward
if test "$(hostname)" = "jubold"; then
	xmodmap -e "keycode 166 = Super_R"
	wicd-client -t &
	pidgin &
	linphone-3 --iconified &
	transmission -m &
fi

# set neo layout as default (to switch use aliases defined in .zshrc)
xmodmap ~/dotfiles/neo_de.xmodmap
# disable beeps
xset b off
exec xmonad

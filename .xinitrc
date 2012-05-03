# ~/.xinitrc
# This trayer is for xmonad + xmobar and holds things as transmission, wicd, etc.
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
    --expand true --width 15 --transparent true --tint 0x191970 --height 12&

# here comes machine dependent stuff, as I currently only have one
# machine, this is pretty straight forward
if test "$(hostname)" = "jubold"; then
    # I don't have a 'win'-key on my IBM T41, that's why I have to
    # remap this one:
    xmodmap -e "keycode 166 = Super_R"
    # wicd is much smaller then network manager
    wicd-client -t &
    pidgin &
    twinkle &
    transmission-gtk --minimized &
    dropbox.py start &
    urxvtd -q -o -f
fi

# set neo layout as default (to switch use aliases defined in .zshrc)
setxkbmap lv
xmodmap "${HOME}/dotfiles/neo_de.xmodmap"
xmodmap -e "keycode 166 = Super_R"
xmodmap "${HOME}/dotfiles/swap_ctrl_altgr.xmodmap"

# disable beeps
xset b off
exec xmonad

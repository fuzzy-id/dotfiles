# ~/.xinitrc
# This trayer is for xmonad + xmobar and holds things as transmission, wicd, etc.
trayer --edge top \
	--align right \
	--SetDockType true \
	--SetPartialStrut false \
	--expand false \
	--widthtype pixel \
	--width 120 \
	--heighttype pixel \
	--height 13 \
	--transparent true \
	--tint 0x191970 &

# set neo layout as default (to switch use aliases defined in .zshrc)
setxkbmap lv
xmodmap "${HOME}/dotfiles/neo_de.xmodmap"
xmodmap "${HOME}/dotfiles/swap_ctrl_altgr.xmodmap"

start_dropbox() {
    local dropboxd="${HOME}/.dropbox-dist/dropboxd"
    if test -e "${dropboxd}"; then
	${dropboxd} start&
    fi
}

# Here comes machine dependent stuff:
case "$(hostname)" in
    "palmo" )
	xmodmap -e "keycode 166 = Super_R"
	wicd-client -t&    
	pidgin&
	twinkle --hide &
	transmission-gtk --minimized &
	start_dropbox
	urxvtd -q -o -f
	;;
    "taris" )
	pidgin&
	start_dropbox
	urxvtd -q -o -f
esac

# disable beeps
xset b off
exec xmonad

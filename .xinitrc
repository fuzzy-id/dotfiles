# ~/.xinitrc
# This trayer is for xmonad + xmobar and holds things as transmission, wicd, etc.
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
    --expand true --width 15 --transparent true --tint 0x191970 --height 12&

# set neo layout as default (to switch use aliases defined in .zshrc)
setxkbmap lv
xmodmap "${HOME}/dotfiles/neo_de.xmodmap"
xmodmap "${HOME}/dotfiles/swap_ctrl_altgr.xmodmap"

function start-dropbox {
    local dropboxd="${HOME}/.dropbox-dist/dropboxd"
    if [[ -e "${dropboxd}" ]]; then
	${dropboxd} start&
    fi
}

# Here comes machine dependent stuff:
case "$(hostname)" in
    "jubold" )
	wicd-client -t &
	pidgin &
	twinkle &
	transmission-gtk --minimized &
	dropbox.py start &
	urxvtd -q -o -f
	;;
    "roku" )
	xmodmap -e "keycode 166 = Super_R"
	wicd-client -t&    
	pidgin&
	start-dropbox
	urxvtd -q -o -f
	;;
    "torux" )
	pidgin&
	urxvtd -q -o -f
esac

# disable beeps
xset b off
exec xmonad

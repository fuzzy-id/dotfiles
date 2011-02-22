trayer --edge top --align right --SetDockType true --SetPartialStrut true \
    --expand true --width 10 --transparent true --tint 0x191970 --height 12&
wicd-gtk&
transmission --minimized &
xmodmap ~/dotfiles/neo_de.xmodmap
xset b off
exec xmonad

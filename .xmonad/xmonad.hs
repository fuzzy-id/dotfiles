import XMonad
import XMonad.Hooks.DynamicLog

main = xmonad =<< xmobar myConfig

myConfig = defaultConfig {
  terminal = "urxvt",
  modMask = mod4Mask,
  normalBorderColor = "black" }
